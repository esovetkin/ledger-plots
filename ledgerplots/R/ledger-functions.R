#' @import stats
#' @importFrom utils "head" "read.csv" "tail"

#' @title Make quries to ledger
#'
#' @description Call a shell and execute ledger command with a given
#'   query.
#'
#' @param query string query that is used in the ledger call.
#'
#' @param options string of options given to ledger
#'
#' @param ledger.path path to the ledger executable
#'
#' @return data.frame containing the following columns
#'   \code{"Date",NA,"Description","Category","Currency","Amount",NA,"Notes"}
#'
#' @details The function make the following call in shell:
#'   \code{ledger csv <query>}
#'
#' One may pass extra argument to ledger this way.
#'
#' @examples
#' \dontrun{
#' expenses <- read.ledger("^expenses: -X EUR")
#'
#' assets <- read.ledger("^assets: -X EUR")
#' }
#'
#' @export
read.ledger <- function(query, options = "", ledger.path = NULL) {
  ledger.bin <- "ledger"

  # check presence of ledger
  if (!is.null(ledger.path)) {
    ledger.bin <- ledger.path

    if (!file.exists(ledger.bin))
      stop(paste("File",ledger.bin,"is not found!"))
  } else {
    if (0 != system2("command",paste(" -v",ledger.bin), stdout=FALSE,stderr=FALSE))
      stop("Ledger command is not found!")
  }

  # make query to ledger and read to string
  lines <- system(paste(ledger.bin, "csv", query, options), intern=TRUE)

  # convert string to a data.frame
  con <- textConnection(lines)

  # create an empty data.frame
  res <- data.frame(matrix(ncol=8,nrow=0))
  if (length(lines))
    res <- read.csv(con, header=FALSE)
  close(con)

  # convert Date to a real Dates
  res[,1] <- as.Date(res[,1])

  # rename columns
  colnames(res) = c("Date",NA,"Description","Category","Currency","Amount",NA,"Notes")

  res
}

#' @title Plot accounts for a given query
#'
#' @description For a given query plot accounts given transaction
#'   function (see plot.ledger). All plots are done in the current
#'   device.
#'
#' @param query query string that is used in read.ledger function to
#'   query the ledger for transactions
#'
#' @param order.depth if do ordering by account depths first
#'
#' @param order.function function that is calculated in order to sort
#'   the plots for different accounts. The function must take a vector
#'   and return a single value
#'
#' @param max.num.plots maximum number of plots
#'
#' @param ledger.options extra options specified during the call to
#'   ledger
#'
#' @param ledger.path path to the ledger executable
#'
#' @param conversion volume unit conversion rules
#'
#' @param type type of plot to make (which amount to consider)
#'
#' @param categorise_by the way plot are categorised by. In case of
#'   tags only accounts with depth 1 are considered
#'
#' @param account_depth_for_each_tag depth of accounts considered for
#'   each tag (in case categorise_by == "tags")
#'
#' @param max_tags_tuples maximum length of tags combinations
#'
#' @param min_tags_entries ignore plots that have less than 5 tags
#'   entries
#'
#' @param ... extra arguments given to plot.ledger function
#'
#' @details the plots are ordered by the depth of the ledger account
#'   name and by a value of the order.function (by default it
#'   calculates the sum of absolute values of transactions). This
#'   allows to order many plots in such a fashion that firstly a
#'   presented a high level plots with the most impact (wrt
#'   order.function).
#'
#' @return list of ggplots
#'
#' @export
query.plot <- function(query,
                       type = c("amount","price","volume","revalued"),
                       categorise_by = c("account","tags","alluvial","pie"),
                       order.depth = TRUE,
                       order.function = function(x) sum(abs(x)),
                       max.num.plots,
                       ledger.options, ledger.path = NULL,
                       conversion = c("1kg = 1000g"),
                       account_depth_for_each_tag = 1,
                       max_tags_tuples = 3,
                       min_tags_entries = 5,
                       ...) {
  type <- match.arg(type)
  categorise_by <- match.arg(categorise_by)

  # read transactions
  cat(paste("Reading transactions for the query:",query,ledger.options,"\n"))
  transactions <- read.ledger(query = query, options = ledger.options,
                              ledger.path = ledger.path)

  # parse notes
  if ( "price" == type || "volume" == type ) {
    transactions <- parse.notes(transactions, conversion)

    # remove first NA values
    transactions <-
      transactions[head(which(cumsum(!is.na(transactions$Volume)) > 0),1):
                     nrow(transactions),]
  }

  if ( "revalued" == type ) {
    transactions <- transactions[grep("<Revalued>", transactions$Category,fixed=TRUE),]
  }

  # in case there is no transactions
  if (! nrow(transactions))
      return(list())

  if ( "pie" == categorise_by ) {
      return(pie.plots(transactions, title = query))
  }

  # get account tree
  tree <- account.tree.depth(transactions$Category)

  if ("tags" == categorise_by) {
    tree <- tree[tree$Depth <= account_depth_for_each_tag,]

    tags <- get_tuples_tags(get_tags(transactions))
  }

  # get ordering of the accounts
  ord <- order(if (order.depth) -tree[,2] else rep(0,nrow(tree)),
               sapply(tree[,1], function(x) {
                 idx <- grep(x,transactions$Category,fixed=TRUE)

                 v <- transactions$Amount[idx]

                 if ( "price" == type) {
                   v <- transactions$Price[idx]
                 }

                 if ( "volume" == type) {
                   v <- transactions$Volume[idx]
                 }

                 order.function(v)
               }),
               decreasing = TRUE)

  # plot only first max.num.plots
  ord <- head(ord, n = max.num.plots)

  # make a plots in the selected order
  plots <- lapply(ord, function(i) {
    cat(paste("Plotting:",tree[i,1],"\n"))
    idx <- grep(tree[i,1],transactions$Category,fixed=TRUE)

    if ("account" == categorise_by) {
      res <- account.plot(X=transactions[idx,],
                          title=tree[i,1],
                          type = type,
                          date.interval = c(min(transactions$Date),
                                            min(Sys.Date(),max(transactions$Date))),
                          ...)
    }

    if ("alluvial" == categorise_by) {
      # get list of accounts of the current substree
      categorise_accounts <- tree[(tree$Depth == tree[i,"Depth"] + 1) & grepl(tree[i,1],tree[,1],fixed=TRUE),1]

      res <- account.plot(X=transactions[idx,],
                          title=tree[i,1],
                          type = type,
                          categorise_accounts = categorise_accounts,
                          date.interval = c(min(transactions$Date),
                                            min(Sys.Date(),max(transactions$Date))),
                          ...)
    }

    if ("tags" == categorise_by) {
      res <- lapply(1:length(tags), function(j)
      {
        name <- paste(c(as.character(tree[i,1]),
                        paste0("%",strsplit(names(tags)[j],".",fixed=TRUE)[[1]])),
                      collapse=" & ")
        cat(paste("Plotting:",name,"\n"))

        idx2 <- tags[[j]]
        idx2 <- idx[idx %in% idx2[idx2 %in% idx]]

        account.plot(X=transactions[idx2,],
                     title = name,
                     type = type,
                     date.interval = c(min(transactions$Date),
                                       min(Sys.Date(),max(transactions$Date))),
                     ...)
      })

      res <- unlist(res, recursive = FALSE)
    }

    res
  })

  # remove recursive lists
  unlist(plots, recursive = FALSE)
}

#' @title Complete data with all dates
#'
#' @param X dataframe with columns "Date" and "Amount"
#'
#' @param dates.series a vector of dates. Entries are added to X with
#'   the dates corresponding to dates.series
#'
#' @param duplicated_transactions functions with which the several
#'   transactions in one day are resolved
#'
#' @param FUN a character vector of function names to be applied to
#'   transactions. Each function creates a column in transaction
#'   data.frame
#'
#' @return a data.frame with "Date" and columns corresponding ot the
#'   FUN names. The data.frame contains a value for each date in dates.series
#'
#' @export
complete_dataframe_missing_dates <- function(X, dates.series,
                                             duplicated_transactions,FUN)
{
  # remove present dates
  dates.series <- dates.series[ !(dates.series %in% X[,"Date"])]

  data <- data.frame("Date"=dates.series,"Amount"=rep(0,length(dates.series)))
  data <- rbind(data,X[,c("Date","Amount")])
  data <- aggregate(data[,2],FUN=duplicated_transactions,
                    by=list(data[,1]))

  colnames(data) <- c("Date","Amount")

  # evaluate functions on the data
  x <- lapply(FUN,
              function(f)
                do.call(eval(parse(text=f)),
                        args=list(data[,"Amount"])))
  # remove original column
  data[,"Amount"] <- NULL

  # evaluate length of the computed statistic
  m <- max(sapply(x,length))

  # append or prepend NA
  x <- lapply(x,
              function(x) {
                # push data to the most latest available if it is too short
                if (length(x) <= nrow(data))
                  return(c(rep(NA,nrow(data)-length(x)),x,rep(NA,m-nrow(data))))

                return(c(x,rep(NA,m-length(x))))
              })
  x <- simplify2array(x)

  # add extra dates
  m_date_old <- max(data[,"Date"])
  data <- rbind(data,
                data.frame("Date"=seq(m_date_old+1,by=1,length.out = m-nrow(data))))


  # append computed values and remove original column
  data[,as.character(FUN)] <- x

  return(data)
}

#' @title Plot ledger data in a current device
#'
#' @description A low lever function that do the actual
#'   plot. Transforms and plots data in some format, providing some
#'   extra information
#'
#' @param X data. First column date, Second column amount
#'
#' @param title title for the plot (account name)
#'
#' @param type type of the plot to make
#'
#' @param categorise_accounts if is not NULL plot alluvial instead of
#'   series
#'
#' @param date.interval dates period between which the plots should be
#'   done (default for one year)
#'
#' @param FUN what to do with data before plotting. This must be a
#'   function that accept a vector as an argument and outputs a
#'   vector.
#'
#' @param ... arguments passed to FUN
#'
#' @return list of ggplot
#'
#' @export
account.plot <- function(X,title,
                         type,
                         categorise_accounts = NULL,
                         date.interval = c(Sys.Date()-365,Sys.Date()),
                         FUN=c("cumsum"),
                         ...) {
  dates.series <- seq(date.interval[1],date.interval[2],1)

  # the way the duplicated transactions in one day are treated
  duplicated_transactions <- sum

  if ("price" == type) {
    X$Currency <- X$Price.curr
    X$Amount <- X$Price
    duplicated_transactions <- mean
  }

  if ("volume" == type) {
    X$Currency <- X$Volume.curr
    X$Amount <- X$Volume
  }

  # ignore items with missing currency
  X <- X[!is.na(X$Currency),]

  if_legend <- FALSE
  if ("if_legend" %in% names(list(...))) {
    if_legend <- list(...)$if_legend
  }

  # check if do alluvial plots
  if (!is.null(categorise_accounts)) {
    if (1 < length(FUN)) {
      warning("FUN has length more than one. Using only the first one for the alluvium plot.")
      FUN <- FUN[1]
    }

    data <- lapply(categorise_accounts, function(account)
    {
      x <- X[grep(account,X$Category,fixed=TRUE),,drop=FALSE]

      x <- lapply(sort(unique(x$Currency)), function(currency)
      {
        res <- complete_dataframe_missing_dates(
          X=x[x$Currency %in% currency,],
          dates.series=dates.series,
          duplicated_transactions=duplicated_transactions,
          FUN=FUN)

        res$Currency <- currency

        res
      })

      x <- do.call(rbind,x)

      if (is.null(x)) {
        return(NULL)
      }

      x$Category <- account

      x
    })

    # remove null entries
    data <- data[!sapply(data,is.null)]

    data <- do.call(rbind,data)

    # simplify label names
    data$Category <- gsub(title,"",data$Category)

    plots <- lapply(sort(unique(data$Currency)), function(currency)
    {
      x <- data[data$Currency == currency,]
      x$Currency <- NULL

      alluvial.plot(data=x,
                    currency=currency,
                    title=title)
    })
  } else {
    # plot for each currency separately
    plots <- lapply(sort(unique(X$Currency)), function(currency)
    {
      data <- complete_dataframe_missing_dates(
        X=X[X$Currency %in% currency,],
        dates.series=dates.series,
        duplicated_transactions=duplicated_transactions,
        FUN=FUN)

      series.plot(data=data,currency=currency,
                  title=title,if_legend=if_legend)
    })
  }

  return(plots)
}

#' @title Low level function that plots the series
#'
#' @param data data.frame to be plotted
#' @param currency type of series values (used as ylab)
#' @param title account name (used as a plot name)
#'
#' @param if_legend boolean variable that controls wheather the legend
#'   is drawn
#'
#' @return ggplot object
#'
#' @export
series.plot <- function(data,currency,title,if_legend=FALSE) {
  data <- reshape2::melt(data,id="Date")

  # main plot
  g <- ggplot2::ggplot(data, ggplot2::aes(x=data$Date,y=data$value,colour=data$variable))
  # line type plot
  g <- g + ggplot2::geom_line()
  # add legend
  if (if_legend) {
    g <- g + ggplot2::theme(legend.position="bottom",
                            legend.text = ggplot2::element_text(size=7),
                            legend.title = ggplot2::element_blank())
  } else {
    g <- g + ggplot2::theme(legend.position="none")
  }
  # minor grid: weeks, major grid: months
  g <- g + ggplot2::theme(panel.grid.minor = ggplot2::element_line(size=0.1),
                          panel.grid.major = ggplot2::element_line(size=0.5)) +
    ggplot2::scale_x_date(minor_breaks = data$Date[weekdays(data$Date) == "Monday"],
                          breaks = data$Date[format(data$Date,"%d") == "01"],
                          date_labels = "%b %Y")
  # labs and title
  g <- g + ggplot2::labs(title=title,x="Date",y=currency) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face="bold"))
  # xlab
  g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, size=5))

  g
}

#' @title pie plot for each currency
#'
#' @param data ledger entries
#'
#' @return list of ggplot objects
#'
#' @export
pie.plots <- function(data, title)
{
    require("ggplot2")
    lapply(sort(unique(data$Currency)), function(currency)
    {
        x <- data[data$Currency %in% currency,]

        pie.plot(data=x) + ggtitle(paste0(title,"; ",currency))
    })
}

#' @title a single pie plot
#'
#' @param data ledger entries
#'
#' @return ggplot object
#'
#' @export
pie.plot <- function(data)
{
    accs <- strsplit(as.character(data$Category),split=":")

    for (i in 1:max(sapply(accs,length))) {
        data[,paste0("Category_",i)] <-
            sapply(accs, function(x)
                if (length(x) < i)
                    paste(x[1:length(x)], collapse=":")
                else
                    paste(x[1:i],collapse=":"))
    }

    cols <- sort(colnames(data)[grep("^Category_[0-9]+$",colnames(data))])

    for (x in cols) {
        d <- aggregate(data$Amount,list(data[,x]),sum)
        rownames(d) <- d$Group.1
        data[,paste0("Amount_",x)] <- d[data[,x],"x"]
    }

    data <- data[do.call(order,data[,paste0("Amount_",cols)]),]

    d <- lapply(1:length(cols), function(i)
    {
        x <- cols[i]
        ymax <- data[! duplicated(data[,x]),paste0("Amount_",x)]
        idx <- ymax > 0
        ymax <- cumsum(ymax[idx])
        ymax <- ymax + 2*(if (min(ymax) < 0) -min(ymax) else 0)
        d <- data.frame("Category" = data[! duplicated(data[,x]),x][idx],
                        "xmin" = i-1,
                        "xmax" = i,
                        "ymax" = ymax,
                        "ymin" = c(0,ymax[-length(ymax)]))
    })
    d <- do.call(rbind,d)

    d$Category <- as.character(d$Category)
    d$size <- (d$ymax - d$ymin) > 2 * (max(d$ymax) - min(d$ymin)) / 360
    d[! d$size,"Category"] <- ""

    require("ggplot2")
    g <- ggplot(d, aes(xmin = xmin, xmax = xmax,
                       ymin = ymin, ymax = ymax)) +
        geom_rect(color="white") +
        geom_text(subset(d,xmax == max(d$xmax)),
                  mapping = aes(x = max(d$xmax), y = (ymax + ymin)/2, label = Category,
                                angle = - 90 - 360 * (ymax + ymin) / 2 / max(d$ymax)
                                ),
                  hjust = 1, size=2) +
        xlim(0,max(d$xmax) + 4) +
        coord_polar(theta = "y") +
        theme_void()

   return(g)
}

#' @title alluvium plots
#'
#' @param data a data.frame with "Date", "Category", "Value" (not
#'   necessarily this name)
#'
#' @param currency currency of the values (put as ylab)
#'
#' @param title plot title
#'
#' @return a ggplot object
#'
#' @export
alluvial.plot <- function(data,currency,title)
{
  data <- reshape2::melt(data,id=c("Date","Category"))

  # convert value column to numeric (lubridate argues)
  data$value <- as.numeric(data$value)

  # remove na data, since alluvium plots require now NA present (NA
  # may happen due to filter function)
  data <- stats::na.omit(data)

  # this removes the CRAN check warning as .data is undeclared
  # variable
  .data <- NULL
  Date <- NULL
  value <- NULL
  Category <- NULL

  # reduce data to a monthly data
  z <- dplyr::summarise(
    dplyr::group_by(
      dplyr::mutate(data,
                    Date = lubridate::floor_date(data$Date,"month")),
      .data$Date, .data$Category),
    value = mean(.data$value))

  # generate a plot
  g <- ggplot2::ggplot(data = z,
                       ggplot2::aes(x = Date,
                                    y = value,
                                    alluvium = Category)) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = Category, colour = Category),
      alpha = .75, decreasing = FALSE)

  # minor grid: weeks, major grid: months
  g <- g + ggplot2::theme(
    panel.grid.minor = ggplot2::element_line(size=0.1),
    panel.grid.major = ggplot2::element_line(size=0.5)) +
    ggplot2::scale_x_date(
      minor_breaks = data$Date[weekdays(data$Date) == "Monday"],
      breaks = data$Date[format(data$Date,"%d") == "01"],
      date_labels = "%b %Y")

  # labs and title
  g <- g + ggplot2::labs(title=title,x="Date",y=currency) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face="bold"))
  # xlab
  g <- g + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle=90, size=5))

  g
}

#' @title Get list of account tree
#'
#' @description For a given character vector of the account names
#'   generate an ordered data.frame containing account names and the
#'   depth of the account.
#'
#' For example, "Assets:Bank:Current" account has depth 3
#'
#' @param names character vector containing different account names
#'
#' @return data.frame containing column account and depth
#'
#' @export
account.tree.depth <- function(names) {
  names <- as.character(names)

  l <- strsplit(names,split=":")

  res <- character(0)

  # do the loop by the depth of account
  for (depth in 1:max(sapply(l,length))) {
    res <- c(res,
             unique(sapply(l,
                           function(x) {
                             paste(x[1:min(depth,length(x))],collapse = ":")
                           })))
  }

  # get unique names
  res <- unique(res)

  res <- data.frame("Account"=res,
                    "Depth" = sapply(res,function(x) length(strsplit(x,":")[[1]])))

  rownames(res) <- 1:nrow(res)

  res
}

#' @title get tags and corresponding indices
#'
#' @description process Notes column from 'ledger csv' and generate a
#'   list with tags and corresponding indices in the data
#'
#' @param data data.frame, output of the read.ledger
#'
#' @return a named list with tags and corresponding indices
#'
#' @export
get_tags <- function(data)
{
  notes <- data$Notes
  notes <- stringr::str_match(notes,"^.*?:([[:alnum:]:]+):.*?$")[,2]

  tags <- unique(unlist(strsplit(notes,":")))

  res <- lapply(tags, function(tag) {
    if (is.na(tag))
      return(integer(0))

    grep(tag,notes,fixed=TRUE)
  })
  names(res) <- tags

  # remove empty tags and NA
  res[is.na(names(res)) | names(res) == ""] <- NULL

  res <- res[order(sapply(res,length),decreasing = TRUE)]

  return(res)
}

#' @title compute different combinations of tags
#'
#' @param tags names list returned by get_tags function
#'
#' @param n maximum number of tuples to consider
#'
#' @param min_num minimum number of entries for the tag combinations
#'   to appear in plots
#'
#' @return named list with indices corresponding to tuples pf tags
#'
#' @export
get_tuples_tags <- function(tags, n=3, min_num=5)
{
  res <- tags
  k <- 2
  tags.tuple <- tags

  while (k <= n) {
    x <- unlist(lapply(tags,function(x)
      lapply(tags.tuple, function(y)
        y[y %in% x[x %in% y]])),recursive=FALSE)

    # remove empty pairs
    x[sapply(x,length) == 0] <- NULL

    # remove "diagonal", i.e. <tag1>.<tag2>.<tag3>.<tag4> when any of
    # tags coincide.
    x[sapply(strsplit(names(x),".",fixed=TRUE),
             function(x) any(duplicated(x)))] <- NULL

    # remove duplicated entries
    x[duplicated(sapply(strsplit(names(x),".",fixed=TRUE),
                        function(x) paste(sort(x),collapse=".")))] <- NULL

    # sort remaining labels
    names(x) <- sapply(strsplit(names(x),".",fixed=TRUE),
                       function(x) paste(sort(x),collapse="."))


    # sort items by popularity
    x <- x[order(sapply(x,length),decreasing=TRUE)]

    res <- append(res,x)
    tags.tuple <- x
    k <- k + 1
  }

  res[sapply(res,length) < min_num] <- NULL

  res
}

#' @title duplicate entries in case there are multiple matches
#'
#' @description a help function. Duplicates entries in a transactions
#'   data.frame. For instance trasaction with note
#'   "20 EUR ; 20l 25kg 10x" will be split onto tree transactions
#'   "20 EUR ; 20l", "20 EUR ; 25kg", "20 EUR ; 10x". In order to
#'   identify that the transaction is being duplicated an extra column
#'   to the data.frame is being added.
#'
#' @param data data.frame, output of the read.ledger
#'
#' @param re regular expression matching "10 kg" like patterns
#'
#' @return data.frame in the simular format as read.ledger
#'
#' @export
parse_duplicate_entries <- function(data,
                                    re = "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)") {
  re.at <- paste0(re,"[ ]?@[ ]?",re)
  re.OR <- paste0(re,"|",re.at)

  # find matches
  matches <- regmatches(data$Notes, gregexpr(re.OR, data$Notes))

  # find length of matches
  l <- sapply(matches,length)

  # duplicate the multiple matches
  x <- rep(1:nrow(data),ifelse(l==0,1,l))
  data <- data[x,]

  # add column for the entries corresponding to re
  matches[l==0] <- NA
  data[,"matches"] <- unlist(matches)

  # add column indicating which entries has been duplicated
  data[,"duplicated_rows"] <- x

  # remove parsed notes
  data$Notes <- gsub(re.OR,"",data$Notes)

  return(data)
}

#' @title parse price and volumes
#'
#' @description get the prices and volumes columns from the parsed entries
#'
#' @param data data.frame
#'
#' @param re regular expression to parse
#'
#' @export
parse_at_entries <- function(data,
                             re = "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)") {
  re.at <- paste0(re,"[ ]?@[ ]?",re)

  x <- as.numeric(gsub(re.at,"\\1",data$matches)) *
    as.numeric(gsub(re.at,"\\3",data$matches))
  x <- as.character(x)
  x[is.na(x)] <- ""

  i.na <- is.na(data$matches)

  data$matches <- paste0(x,gsub(re.at,"\\4",data$matches))
  data$matches[i.na] <- NA

  return(data)
}

#' @title convert units one to another
#'
#' @description convert all right units to left units
#'
#' @param data data frame containing parsed volumes
#'
#' @param conversion vector of containing conversion rules. E.g.:
#' c("1 kg = 1000g","1l = 1000ml")
#'
#' @export
convert.units <- function(data, conversion = c("1kg = 1000g")) {
  for (conv in conversion) {
    re <- "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)"
    re <- paste0(re,"[ ]?=[ ]?",re)

    to.val <- gsub(re,"\\1",conv)
    to.cur <- gsub(re,"\\2",conv)
    from.val <- gsub(re,"\\3",conv)
    from.cur <- gsub(re,"\\4",conv)

    i <- grep(paste0("^",from.cur,"$"), data$Volume.curr)
    data$Volume.curr[i] <- to.cur
    data$Volume[i] <- data$Volume[i]*as.numeric(to.val)/as.numeric(from.val)
  }

  data
}

#' @title Parse comments and get prices and volumes
#'
#' @description Parse transaction notes and get a data frame containing
#' columns: price, price.currency, volume, volume.currency, other comments
#'
#' @param data read ledger data
#'
#' @param conversion unit conversion rules
#'
#' @export
parse.notes <- function(data, conversion = c("1kg = 1000g")) {
  # regular expressio matching entries
  re <- "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)"

  # parse notes and duplicate rows with several entries
  data <- parse_duplicate_entries(data, re)

  # parse @ entries
  data <- parse_at_entries(data, re)

  # calculate volume
  data$Volume <- as.numeric(gsub(re,"\\1",data$matches))
  data$Volume.curr <- gsub(re,"\\2",data$matches)

  # convert g to kg
  data <- convert.units(data, conversion)

  # calculate price
  data$Price <- data$Amount / data$Volume
  data$Price.curr <- paste0(data$Currency,"/",data$Volume.curr)
  data$Price.curr[is.na(data$Volume)] <- NA

  # delete matches colume
  data$matches <- NULL

  return(data)
}

#' Generate a latex table with food prices
#'
#' @param FUN list of functions to call on prices
#'
#' @param ofile output filename
#'
#' @param query ledger query
#'
#' @param ledger.options extra options passed to ledger
#'
#' @param ledger.path path to the ledger binary
#'
#' @param conversion volume unit conversion rules
#'
#' @export
generate.price.table <- function(FUN, query, ofile="food-prices.tex",
                                 ledger.options, ledger.path=NULL,
                                 conversion = c("1kg = 1000g")) {
  # read transactions
  cat(paste("Reading transactions for the query:",query,ledger.options,"\n"))
  data <- read.ledger(query = query, options = ledger.options,
                      ledger.path = ledger.path)

  # parse transaction notes
  data <- parse.notes(data, conversion)

  # lowercase payee
  data$Description <- tolower(data$Description)

  # evaluate some statistics on prices
  prices <- aggregate(data$Price,
                      by=list(data$Category,data$Description,data$Price.curr),
                      function(x) sapply(FUN, function(f) tail(eval(parse(text=f))(x),n=1)))

  # rename columns
  for (i in 1:length(FUN))
    prices[,FUN[i]] <- prices$x[,i]
  prices$x <- NULL
  colnames(prices) <- c("category","payee","currency",FUN)

  # order by category and currency
  data <- prices[grep("*",prices[,1],ignore.case = TRUE),]
  data <- data[do.call(order,split.default(data,colnames(data))[c("category","currency",FUN)]),]

  # positions of the horizontal lines
  hline.after <- c(-1,0,which(diff(as.numeric(data$category)) > 0),nrow(data))

  # write table to the latex file
  write(paste("\\documentclass[a4paper]{article} \\pagestyle{empty}",
              " \\usepackage[utf8]{inputenc} \\usepackage{longtable}",
              " \\usepackage[left=1cm, right=1cm, bottom=1cm,top=1cm]{geometry}",
              "\\begin{document}"),file=ofile)
  print(xtable::xtable(data,digits=2),type="latex",file=ofile,
        append=TRUE,tabular.environment = 'longtable',
        include.rownames=FALSE,floating=FALSE,
        hline.after = hline.after)
  write("\\end{document}",file=ofile,append=TRUE)
}

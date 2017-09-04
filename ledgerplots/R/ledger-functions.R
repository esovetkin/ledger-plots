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
#' expenses <- read.ledger("^expenses: -X EUR")
#'
#' assets <- read.ledger("^assets: -X EUR")
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
                       type = c("amount","price","volume"),
                       order.depth = TRUE,
                       order.function = function(x) sum(abs(x)),
                       max.num.plots,
                       ledger.options, ledger.path = NULL, ...) {
  type <- match.arg(type)

  # read transactions
  cat(paste("Reading transactions for the query:",query,ledger.options,"\n"))
  transactions <- read.ledger(query = query, options = ledger.options,
                              ledger.path = ledger.path)

  # parse notes
  if ("amount" != type) {
    transactions <- parse.notes(transactions)
  }

  # in case there is no transactions
  if (! nrow(transactions))
    return(list())

  # get account tree
  cat("Generating accounts tree...\n")
  tree <- account.tree.depth(transactions$Category)

  # get ordering of the accounts
  ord <- order(if (order.depth) -tree[,2] else rep(0,nrow(tree)),
               sapply(tree[,1], function(x) {
                 idx <- grep(x,transactions$Category)
                 order.function(transactions$Amount[idx])
               }),
               decreasing = TRUE)

  ord <- head(ord, n = max.num.plots)

  # make a plots in the selected order
  plots <- lapply(ord, function(i) {
    cat(paste("Plotting:",tree[i,1],"\n"))
    idx <- grep(tree[i,1],transactions$Category)
    account.plot(X=transactions[idx,],
                 title=tree[i,1],
                 type = type,
                 date.interval = c(min(transactions$Date),min(Sys.Date(),max(transactions$Date))),
                 ...)
  })

  # remove recursive lists
  unlist(plots, recursive = FALSE)
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
                         type = c("amount","price","volume"),
                         date.interval = c(Sys.Date()-365,Sys.Date()),
                         FUN=cumsum, ...) {
  type <- match.arg(type)

  dates.series <- seq(date.interval[1],date.interval[2],1)

  # remove present dates
  dates.series <- dates.series[ !(dates.series %in% X[,"Date"])]

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

  # plot for each currency separately
  lapply(sort(unique(X$Currency)), function(currency) {
    data <- data.frame("Date"=dates.series,"Amount"=rep(0,length(dates.series)))
    data <- rbind(data,X[X$Currency %in% currency,c("Date","Amount")])
    data <- aggregate(data[,2],FUN=duplicated_transactions,
                      by=list(data[,1]))
    data[,2] <- FUN(data[,2],...)
    colnames(data) <- c("Date","Amount")

    series.plot(data,currency=currency,title=title)
  })
}

#' @title Low level function that plots the series
#'
#' @param data data.frame to be plotted
#' @param currency type of series values (used as ylab)
#' @param title account name (used as a plot name)
#'
#' @return ggplot object
#'
#' @export
series.plot <- function(data,currency,title) {
  require("ggplot2", quietly = TRUE)

  # main plot
  g <- ggplot(data, aes(Date,Amount))
  # line type
  g <- g + geom_line(color="firebrick")
  # minor grid: weeks, major grid: months
  g <- g + theme(panel.grid.minor = element_line(size=0.1),
                 panel.grid.major = element_line(size=0.5)) +
    scale_x_date(minor_breaks = data$Date[weekdays(data$Date) == "Monday"],
                 breaks = data$Date[format(data$Date,"%d") == "01"],
                 date_labels = "%b %Y")
  # labs and title
  g <- g + labs(title=title,x="Date",y=currency) +
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
  # xlab
  g <- g + theme(axis.text.x = element_text(angle=90, size=5))

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

#' @title Calculate 30 days average of the given vector
#'
#' @description example of a functions can be used to be appled for
#'   the transaction vectors
#'
#' @param x a numeric vector
#'
#' @export
monthly <- function(x) {
  filter(x,rep(1,30),sides=1)
}

#' @title Monthly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
monthly.price <- function(x) {
  n <- filter(abs(x) > 0,rep(1,30),sides=1)
  n[! abs(n) > 0] <- 1
  filter(x,rep(1,30),sides=1)/n
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


  ## # experiments
  ## data <- read.ledger(query="",options="-f ~/bank/food-ledger_2013.log -f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log -f ~/bank/food-ledger_2016.log -f ~/bank/food-ledger.log")


#' @title Parse comments and get prices and volumes
#'
#' @description Parse transaction notes and get a data frame containing
#' columns: price, price.currency, volume, volume.currency, other comments
#'
#'
parse.notes <- function(data) {
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
  i <- grep("^g$",data$Volume.curr)
  data$Volume.curr[i] <- "kg"
  data$Volume[i] <- data$Volume[i]/1000;

  # calculate price
  data$Price <- data$Amount / data$Volume
  data$Price.curr <- paste0(data$Currency,"/",data$Volume.curr)
  data$Price.curr[is.na(data$Volume)] <- NA

  # delete matches colume
  data$matches <- NULL

  return(data)
}

#' @title estimate average/variance of transaction expense per given
#'   period using Poisson compound process
#'
#' @param data transactions data.frame. Default is one year
#'
#' @param period period in days of which the average is calculated
compound.poisson <- function(data, period = 365) {

  data <- read.ledger(query="",options="-f ~/bank/food-ledger_2013.log -f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log -f ~/bank/food-ledger.log -X EUR")

  data <- data[grep("Food:Cookies",data$Category),c("Date","Amount")]

  # remove zero period time jumps
  data <- aggregate(data$Amount, by = list("Date"=data$Date), FUN = sum)

  lambda.t <- (period/as.numeric(mean(diff(data[,1]))))

  c(lambda.t*mean(data[,2]), sqrt(lambda.t*mean(data[,2]^2)))
}

#' Generate a latex table with food prices
#'
#' @param ofile output filename
#'
#' @param query ledger query
#'
#' @param ledger.options extra options passed to ledger
#'
#' @param ledger.path path to the ledger binary
#'
#' @export
generate.price.table <- function(query, ofile="food-prices.text",
                                 ledger.options, ledger.path=NULL) {
  # read transactions
  cat(paste("Reading transactions for the query:",query,ledger.options,"\n"))
  data <- read.ledger(query = query, options = ledger.options,
                      ledger.path = ledger.path)

  # experiments
  data <- read.ledger(
    paste("food house bath other -f ~/bank/food-ledger_2013.log ",
          "-f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log ",
          "-f ~/bank/food-ledger_2016.log ",
          "-f ~/bank/food-ledger.log -H -X EUR -b 2016-03-12"))

  # parse transaction notes
  data <- parse.notes(data)

  # evaluate some statistics on prices
  prices <- aggregate(data$Price,by=list(data$Category,data$Description,data$Price.curr),
                      function(x) c("min"=min(x),"mean"=mean(x),"last"=tail(x,1)))

  # rename column names
  prices$min <- prices$x[,1]
  prices$mean <- prices$x[,2]
  prices$last <- prices$x[,3]
  prices$x <- NULL
  colnames(prices) <- c("category","shop","currency","min","mean","last")

  # order by category and currency
  data <- prices[grep("*",prices[,1],ignore.case = TRUE),]
  data <- data[order(data$category,data$min,data$currency),]

  # positions of the horizontal lines
  hline.after <- c(-1,0,which(diff(as.numeric(data$category)) > 0),nrow(data))

  require(xtable, quietly = TRUE)

  # write table to the latex file
  write(paste("\\documentclass[a4paper]{article} \\pagestyle{empty}",
              " \\usepackage[utf8]{inputenc} \\usepackage{longtable}",
              " \\usepackage[left=1cm, right=1cm, bottom=1cm,top=1cm]{geometry}",
              "\\begin{document}",file=ofile))
  print(xtable(data,digits=5),type="latex",file=ofile,
        append=TRUE,tabular.environment = 'longtable',
        include.rownames=FALSE,floating=FALSE,
        hline.after = hline.after)
  write("\\end{document}",file=ofile,append=TRUE)
}

#' Convert comments in food ledger to the corresponding prices
#' @param food dataset with food, returns by read.ledger
#' @param currency sometimes currency in transaction note is given
#'
#' @export
food.prices.convert <- function(food,currency) {
  # convert names of the shops to a lower case
  food$Description <- tolower(food$Description)

  # paying currency and amount
  food$Currency <- as.character(food$Currency)
  food$Amount <- as.numeric(food$Amount)

  # add price vectors
  food$Price <- rep(NA,nrow(food))
  food$Price.currn <- rep(NA,nrow(food))

  # Convert comments to character
  food$Notes <- as.character(food$Notes)

  food$Volume <- rep(NA,nrow(food))
  food$Volume.Curr <- rep("",nrow(food))

  ## deal with comments like "0.5kg @ 1.99 EUR" or "10x @ 100g"
  regstr <- "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)[ ]?@[ ]?([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)"
  str <- regmatches(food$Notes,gregexpr(regstr,food$Notes))

  # add extra lines in food \todo remove repetition
  l <- sapply(str,length)
  idx <- rep(1,nrow(food))
  idx[which(l>1)] <- l[l>1]
  idx.at <- l[rep(1:nrow(food),idx)] > 0
  food <- food[rep(1:nrow(food),idx),]
  str <- unlist(str)

  # obtain the values and currencies
  at.value <- as.numeric(gsub(regstr,"\\3",str))
  at.currn <- gsub(regstr,"\\4",str)

  unit.value <- as.numeric(gsub(regstr,"\\1",str))
  unit.currn <- gsub(regstr,"\\2",str)

  # index of recorded prices, i.e. currency coincide
  idx.rec <- at.currn == food$Currency[idx.at]

  food$Price.currn[idx.at][idx.rec] <-
    paste("\"",at.currn[idx.rec],"/",unit.currn[idx.rec],"\"",sep="")
  food$Price[idx.at][idx.rec] <- at.value[idx.rec]

  food$Volume[idx.at][idx.rec] <- unit.value[idx.rec]
  food$Volume.Curr[idx.at][idx.rec] <- unit.currn[idx.rec]

  # for others the following rule applies
  food$Price[idx.at][!idx.rec] <-
    food$Amount[idx.at][!idx.rec] / (at.value[!idx.rec]*unit.value[!idx.rec])
  food$Price.currn[idx.at][!idx.rec] <-
    paste("\"",food$Currency[idx.at][!idx.rec],"/",at.currn[!idx.rec],"\"",sep="")

  food$Volume[idx.at][!idx.rec] <- unit.value[!idx.rec]
  food$Volume.Curr[idx.at][!idx.rec] <- unit.currn[!idx.rec]

  # remove captured items
  food$Notes <- gsub(regstr,"",food$Notes)

  ## deal with prices like " @ 0.99 EUR/kg"
  regstr <- "@[ ]?([0-9]+[.0-9]*)[ ]?([[:alpha:]]+/[[:alpha:]]+)"
  m.at <- regexpr(regstr,food$Notes)
  idx.at <- m.at != -1
  str <- regmatches(food$Notes,m.at)

  food$Price[idx.at] <- as.numeric(gsub(regstr,"\\1",str))
  food$Price.currn[idx.at] <- paste("\"",gsub(regstr,"\\2",str),"\"",sep="")

  # remove captured items
  food$Notes <- gsub(regstr,"",food$Notes)

  ## deal with other amounts
  regstr <- "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)"
  str <- regmatches(food$Notes,gregexpr(regstr,food$Notes))

  # add extra lines in food
  l <- sapply(str,length)
  idx <- rep(1,nrow(food))
  idx[which(l>1)] <- l[l>1]
  idx.at <- l[rep(1:nrow(food),idx)] > 0
  food <- food[rep(1:nrow(food),idx),]
  str <- unlist(str)

  # get value and currency
  unit.value <- as.numeric(gsub(regstr,"\\1",str))
  unit.currn <- gsub(regstr,"\\2",str)

  food$Price[idx.at] <- food$Amount[idx.at]/as.numeric(unit.value)
  food$Price.currn[idx.at] <-
    paste("\"",food$Currency[idx.at],"/",unit.currn,"\"",sep="")

  food$Volume[idx.at] <- unit.value
  food$Volume.Curr[idx.at] <- unit.currn

  # remove capture items. So far I have covered all cases above
  food$Notes <- gsub(regstr,"",food$Notes)

  # write data
  food$Currency <- food$Price.currn
  food$Amount <- food$Price

  food <- food[!is.na(food$Amount),]

  # convert g to kg
  food[food$Currency == paste("\"",currency,"/g\"",sep=""),"Amount"] <-
    food[food$Currency == paste("\"",currency,"/g\"",sep=""),"Amount"]*1000
  food[food$Currency == paste("\"",currency,"/g\"",sep=""),"Currency"] <-
    paste("\"",currency,"/kg\"",sep="")

  # remove extra columns
  food <- food[, !(colnames(food) %in% c("Price","Price.currn"))]

  return(food)
}

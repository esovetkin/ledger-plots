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
#' @param order.function function that is calculated in order to sort
#'   the plots for different accounts. The function must take a vector
#'   and return a single value
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
#' @return nothing
#'
#' @export
queryplot <- function(query, order.function = function(x) sum(abs(x)),
                      ledger.options, ledger.path = NULL, ...) {
  # read transactions
  cat(paste("Reading transactions for the query:",query,"\n"))
  transactions <- read.ledger(query = query, options = ledger.options,
                              ledger.path = ledger.path)

  # get account tree
  cat("Generating accounts tree...\n")
  tree <- account.tree.depth(transactions$Category)

  # get ordering of the accounts
  ord <- order(-tree[,2],
               sapply(tree[,1], function(x) {
                 idx <- grep(x,transactions$Category)
                 order.function(transactions$Amount[idx])
               }),
               decreasing = TRUE)

  # make a plots in the selected order
  for (i in ord) {
    cat(paste("Plotting:",tree[i,1],"\n"))
    idx <- grep(tree[i,1],transactions$Category)
    transactionplots(X=transactions[idx,c(1,6)],
                     title=tree[i,1],
                     date.interval = c(min(transactions$Date),max(transactions$Date)),
                     ...)
  }
}

#' @title Plot ledger data in a current device
#'
#' @description A low lever function that do the actual
#'   plot. Transforms and plots data in some format, providing some
#'   extra information
#'
#' @param X data. First column date, Second column amount
#'
#' @param title title for the plot
#'
#' @param date.interval dates period between which the plots should be
#'   done
#'
#' @param FUN what to do with data before plotting. This must be a
#'   function that accept a vector as an argument and outputs a
#'   vector.
#'
#' @param ... arguments passed to FUN
#'
#' @return nothing
#'
#' @examples
#' # read expenses data
#' expenses <- read.ledger("^expenses: -X EUR")
#'
#' # plot cumsum over 30 days expenses
#' plot.ledger(X=expenses[,c(1,6)],
#'             title="30 days average expenses",
#'             FUN=filter, rep(1,30),sides=1)
#'
#' @export
transactionplots <- function(X,title,
                             date.interval = c(Sys.Date()-365,Sys.Date()),
                             FUN=cumsum,...) {
  dates.series <- seq(date.interval[1],date.interval[2],1)

  data <- data.frame("Date"=dates.series,"Amount"=0)
  data <- rbind(data,X)
  data <- aggregate(data[,2],FUN=sum,by=list(data[,1]))
  data[,2] <- FUN(data[,2],...)

  plot(data[,1],data[,2],
       type = "l", main = title,
       ylab = "",xlab = "",
       xaxt = 'n',yaxt = 'n')
  axis(side=2,
       at=seq(min(data[,2],na.rm=TRUE),max(data[,2],na.rm=TRUE),length.out = 15),
       labels=round(seq(min(data[,2],na.rm=TRUE),max(data[,2],na.rm=TRUE),length.out = 15)),
       las=2)
  axis(side=1,
       at=data[seq(1,nrow(data),length.out = 15),1],
       labels=data[seq(1,nrow(data),length.out = 15),1],
       las=3)
  grid(nx=ceiling(as.numeric(max(data[,1])-min(data[,1]))/30), ny=25, col="black")
  abline(h=0, col=2)
  legend("topleft",
         paste("ny cell=",
               round((max(data[,2],na.rm=TRUE)-min(data[,2],na.rm=TRUE))/25),
               sep=""),
         bty="n")
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

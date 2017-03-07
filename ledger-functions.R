##! Function to read from ledger

##! \param string string passes to ledger command. The precise string
##! which is called in shell is the following:
##! > ledger csv <string>
##! One may pass extra argument to ledger this way, see examples.
read.ledger <- function(string) {
    lines <- system(paste("ledger csv",string,sep=" "),
                    intern=TRUE)
    
    con <- textConnection(lines)
    res <- read.csv(con, header=FALSE)
    close(con)
    res[,1] <- as.Date(res[,1])
    
    colnames(res) = c("Date",NA,"Description","Category","Currency","Amount",NA,"Notes")

    res
}


##! Funciton to plot ledger data
##! Transforms and plots data in some format, providing some extra information
##!
##! \param X data. First column date, Second column amount
##!
##! \param title title for plot
##!
##! \param FUN what to do with data before plotting
##!
##! \param ... arguments passed to FUN
##!
##! \return nothing
plot.ledger <- function(X,title,FUN=cumsum,...) {
    dates.series <- seq(as.Date("2013-09-01"),Sys.Date(),1)
    
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

#! Get list of categories and subcatogories
#!
#! \param names character vector containing different account names
#!
#! The purpose of the function is to return all possible names on each
#! level. Function returns list of categories names ordered by the
#! depth
subcategories <- function(names) {
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

##! Convert comments in food ledger to the corresponding prices
##! \param food dataset with food, returns by read.ledger
##! \param currency sometimes currency in transaction note is given
food.prices.convert <- function(food,currency) {
    ## convert names of the shops to a lower case
    food$Description <- tolower(food$Description)
    
    ## paying currency and amount
    food$Currency <- as.character(food$Currency)
    food$Amount <- as.numeric(food$Amount)
    
    ## add price vectors
    food$Price <- rep(NA,nrow(food))
    food$Price.currn <- rep(NA,nrow(food))
    
    ## Convert comments to character
    food$Notes <- as.character(food$Notes)

### deal with comments like "0.5kg @ 1.99 EUR" or "10x @ 100g" 
    regstr <- "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)[ ]?@[ ]?([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)"
    str <- regmatches(food$Notes,gregexpr(regstr,food$Notes))

    ## add extra lines in food \todo remove repitition
    l <- sapply(str,length)
    idx <- rep(1,nrow(food))
    idx[which(l>1)] <- l[l>1]
    idx.at <- l[rep(1:nrow(food),idx)] > 0
    food <- food[rep(1:nrow(food),idx),]
    str <- unlist(str)

    ## obtain the values and currencies
    at.value <- as.numeric(gsub(regstr,"\\3",str))
    at.currn <- gsub(regstr,"\\4",str)

    unit.value <- as.numeric(gsub(regstr,"\\1",str))
    unit.currn <- gsub(regstr,"\\2",str)

    ## index of recorded prices, i.e. currency coincide
    idx.rec <- at.currn == food$Currency[idx.at]

    food$Price.currn[idx.at][idx.rec] <-
        paste("\"",at.currn[idx.rec],"/",unit.currn[idx.rec],"\"",sep="")
    food$Price[idx.at][idx.rec] <- at.value[idx.rec]

    ## for others the following rule applies
    food$Price[idx.at][!idx.rec] <-
        food$Amount[idx.at][!idx.rec] / (at.value[!idx.rec]*unit.value[!idx.rec])
    food$Price.currn[idx.at][!idx.rec] <- 
        paste("\"",food$Currency[idx.at][!idx.rec],"/",at.currn[!idx.rec],"\"",sep="")

    ## remove captured items
    food$Notes <- gsub(regstr,"",food$Notes)

### deal with prices like " @ 0.99 EUR/kg"
    regstr <- "@[ ]?([0-9]+[.0-9]*)[ ]?([[:alpha:]]+/[[:alpha:]]+)"
    m.at <- regexpr(regstr,food$Notes)
    idx.at <- m.at != -1
    str <- regmatches(food$Notes,m.at)

    food$Price[idx.at] <- as.numeric(gsub(regstr,"\\1",str))
    food$Price.currn[idx.at] <- paste("\"",gsub(regstr,"\\2",str),"\"",sep="")

    ## remove captured items
    food$Notes <- gsub(regstr,"",food$Notes)

### deal with other amounts
    regstr <- "([0-9]+[.0-9]*)[ ]?([[:alpha:]]+)"
    str <- regmatches(food$Notes,gregexpr(regstr,food$Notes))

    ## add extra lines in food
    l <- sapply(str,length)
    idx <- rep(1,nrow(food))
    idx[which(l>1)] <- l[l>1]
    idx.at <- l[rep(1:nrow(food),idx)] > 0
    food <- food[rep(1:nrow(food),idx),]
    str <- unlist(str)

    ## get value and currency
    unit.value <- as.numeric(gsub(regstr,"\\1",str))
    unit.currn <- gsub(regstr,"\\2",str)

    food$Price[idx.at] <- food$Amount[idx.at]/as.numeric(unit.value)
    food$Price.currn[idx.at] <-
        paste("\"",food$Currency[idx.at],"/",unit.currn,"\"",sep="")

    ## remove capture items. So far I have covered all cases above
    food$Notes <- gsub(regstr,"",food$Notes)

    ## write data
    food$Currency <- food$Price.currn
    food$Amount <- food$Price

    food <- food[!is.na(food$Amount),]

    ## convert g to kg
    food[food$Currency == paste("\"",currency,"/g\"",sep=""),"Amount"] <-
        food[food$Currency == paste("\"",currency,"/g\"",sep=""),"Amount"]*1000
    food[food$Currency == paste("\"",currency,"/g\"",sep=""),"Currency"] <-
        paste("\"",currency,"/kg\"",sep="")

    ## remove extra columns
    food <- food[, !(colnames(food) %in% c("Price","Price.currn"))]
    
    return(food)
}

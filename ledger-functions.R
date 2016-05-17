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
    
    colnames(res) = c("Date",NA,"Description","Category","Currency","Amount",NA,"Note")

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
plot.ledger <- function(X,title,start.date=Sys.Date()-365*2,FUN=cumsum,...) {
    dates.series <- seq(start.date,Sys.Date(),1)

    #X <- X[X[,1] >= min(dates.series),]
    #X <- rbind(X,data.frame("Date"=min(dates.series),"Amount"=sum(X[X[,1] < min(dates.series),2])))
    
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
    abline(v=Sys.Date() - 30, col="red")
    grid(nx=ceiling(as.numeric(max(data[,1])-min(data[,1]))/30), ny=25, col="black")
    abline(h=0, col=2)
    legend("topleft",
           paste("ny cell=",
                 round((max(data[,2],na.rm=TRUE)-min(data[,2],na.rm=TRUE))/25),
                 sep=""),
           bty="n")   
}

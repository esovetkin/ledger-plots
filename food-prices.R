setwd("~/bank/")
source("ledger-functions.R")

## read assets data
food <- read.ledger(paste("food house bath other -f ~/bank/food-ledger_2013.log ",
                          "-f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log ",
                          "-f ~/bank/food-ledger.log -H -X EUR -b 2016-03-15"))

## paying currency and amount
Currency <- as.character(food$Currency)
Amount <- as.numeric(food$Amount)

## Price vector
Price <- rep(NA,nrow(food))
Price.currn <- rep(NA,nrow(food))

## Convert comments to character
Notes <- as.character(food$Note)

## prices like "0.5kg @ 1.99 EUR" or "10x @ 100g"
regstr <- "([.0-9]+)[ ]*([[:alpha:]]+)[ ]*@[ ]*([.0-9]+)[ ]*([[:alpha:]]+)"
m.at <- regexpr(regstr,Notes)
idx.at <- m.at != -1
str <- regmatches(Notes,m.at)

at.value <- as.numeric(gsub(regstr,"\\3",str))
at.currn <- gsub(regstr,"\\4",str)

unit.value <- as.numeric(gsub(regstr,"\\1",str))
unit.currn <- gsub(regstr,"\\2",str)

## index of recorded prices, i.e. currency coincide
idx.rec <- at.currn == Currency[idx.at]

Price.currn[idx.at][idx.rec] <-
    paste("\"",at.currn[idx.rec],"/",unit.currn[idx.rec],"\"",sep="")
Price[idx.at][idx.rec] <- at.value[idx.rec]

## for others the following rule applies
Price[idx.at][!idx.rec] <- Amount[idx.at][!idx.rec] / (at.value[!idx.rec]*unit.value[!idx.rec])
Price.currn[idx.at][!idx.rec] <- 
    paste("\"",Currency[idx.at][!idx.rec],"/",at.currn[!idx.rec],"\"",sep="")

## remove captured items
Notes <- gsub(regstr,"",Notes)

## prices like " @ 0.99 EUR/kg"
regstr <- "@[ ]*([.0-9]+)[ ]*([[:alpha:]]+/[[:alpha:]]+)"
m.at <- regexpr(regstr,Notes)
idx.at <- m.at != -1
str <- regmatches(Notes,m.at)

Price[idx.at] <- as.numeric(gsub(regstr,"\\1",str))
Price.currn[idx.at] <- paste("\"",gsub(regstr,"\\2",str),"\"",sep="")


## remove captured items
Notes <- gsub(regstr,"",Notes)

## deal with other amounts
regstr <- "([.0-9]+)[ ]*([[:alpha:]]+)"
m.at <- regexpr(regstr,Notes)
idx.at <- m.at != -1
str <- regmatches(Notes,m.at)
str

unit.value <- as.numeric(gsub(regstr,"\\1",str))
unit.currn <- gsub(regstr,"\\2",str)

Price[idx.at] <- Amount[idx.at]/as.numeric(unit.value)
Price.currn[idx.at] <-
    paste("\"",Currency[idx.at],"/",unit.currn,"\"",sep="")

## remove capture items. So far I have covered all cases above
Notes <- gsub(regstr,"",Notes)

## write data
food$Currency <- Price.currn
food$Amount <- Price

food <- food[!is.na(food$Amount),]

food[food$Currency == "\"EUR/g\"","Amount"] <-
    food[food$Currency == "\"EUR/g\"","Amount"]*1000
food[food$Currency == "\"EUR/g\"","Currency"] <- "\"EUR/kg\""


prices <- aggregate(food$Amount,by=list(food$Category,food$Description,food$Currency),min)
colnames(prices) <- c("category","shop","currency","price")

data <- prices[grep("*",prices[,1],ignore.case = TRUE),]
data <- data[order(data$category,data$price,data$currency),]

require(xtable)

write("\\documentclass[a4paper]{article} \\pagestyle{empty} \\usepackage{longtable} \\usepackage[left=1cm, right=1cm, bottom=1cm,top=1cm]{geometry} \\begin{document}",file="food-prices.tex")
print(xtable(data,digits=5),type="latex",file="food-prices.tex",append=TRUE,tabular.environment = 'longtable',include.rownames=FALSE,floating=FALSE)
write("\\end{document}",file="food-prices.tex",append=TRUE)

system("pdflatex food-prices.tex && pdflatex food-prices.tex")

## set you working directory
setwd("~/bank/")
source("ledger-functions.R")

pdf("assets-expenses.pdf", width = 13, height = 7, onefile = TRUE)

## graphical parameters
par(mex=1.2,mar=c(6,4,1,1), mfcol=c(2,2))

##################
## assets plots ##
##################

## read assets data
assets <- read.ledger("assets")

## plot total amounts
plot.ledger(X=assets[,c(1,6)],title="Total Assets",FUN=cumsum)

## split assets on its categories
assets.split <- split(assets,assets[,4])

## this allows to plot first the most contributed assets/expenses
ord <- order(sapply(assets.split, function(x) sum(-x$Amount)))
for (i in ord) {
    plot.ledger(X=assets.split[[i]][,c(1,6)],
                  title=names(assets.split)[i],
                  FUN=cumsum)
}

####################
## expenses plots ##
####################


## read expenses data
expenses <- read.ledger("expenses")

## total expenses plot
plot.ledger(X=expenses[,c(1,6)],title="Total Expenses",
              FUN=filter, rep(1,30),sides=1)

## remove some subtrees from accounts. For example, I also store
## information in which shop I did my groceries.
categories <- as.character(expenses$Category)
categories <- gsub("Expenses:Food:Grocery_and_householding.*",
                   "Expenses:Food:Grocery_and_householding",
                   categories)
expenses$Category <- as.factor(categories)

# split assets on its categories
expenses.split <- split(expenses,expenses[,4])

## this allows to plot first the most contributed assets/expenses
ord <- order(sapply(expenses.split, function(x) sum(-x$Amount)))
for (i in ord) {
    plot.ledger(X=expenses.split[[i]][,c(1,6)],
                  title=names(expenses.split)[[i]],
                  FUN=filter, rep(1,30),sides=1)
}


dev.off()

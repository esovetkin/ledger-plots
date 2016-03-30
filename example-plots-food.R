setwd("~/bank/")
source("plots-functions.R")

## plots for food
pdf("food.pdf", width = 13, height = 7, onefile = TRUE)

par(mex=1.2,mar=c(6,4,1,1), mfcol=c(2,2))

################
## food plots ##
################

## in a different ledger files I store what kind of food I was buying
food <- read.ledger("food house bath other -f ~/bank/food-ledger_2013.log -f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log -f ~/bank/food-ledger.log")

## plot total of food
plot.ledger(X=food[,c(1,6)],
              title="Total",
              FUN=filter,rep(1,30),sides=1)

## split food on its categories
food.split <- split(food,food[,4])

## this allows to plot first the most contributed assets/expenses
ord <- order(sapply(food.split, function(x) sum(-x$Amount)))
for (i in ord) {
    plot.ledger(X=food.split[[i]][,c(1,6)],
                  title=names(food.split)[i],
                  FUN=filter,rep(1,30),sides=1)
}

dev.off()

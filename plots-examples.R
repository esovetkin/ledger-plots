source("ledger-functions.R")

pdf("assets-expenses.pdf", width = 13, height = 7, onefile = TRUE)

## graphical parameters
par(mex=1.2,mar=c(6,4,1,1), mfcol=c(2,2))

plot.query("^assets: -X EUR", FUN=cumsum)

plot.query("^expenses: -X EUR", FUN=function(x) filter(x,rep(1,30),sides=1))

plot.query("^income: -X EUR", FUN=function(x) -cumsum(x))

dev.off()

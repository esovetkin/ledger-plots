setwd("~/bank/")
source("ledger-functions.R")

currency <- "EUR"

## read assets data
food <- read.ledger(paste("food house bath other -f ~/bank/food-ledger_2013.log ",
                          "-f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log ",
                          "-f ~/bank/food-ledger.log -H -X ",currency," -b 2016-03-15"))


food <- food.prices.convert(food,currency)

#### evaluate some statistics on prices
prices <- aggregate(food$Amount,by=list(food$Category,food$Description,food$Currency),
                    function(x) c("min"=min(x),"mean"=mean(x),"max"=max(x)))
prices$min <- prices$x[,1]
prices$mean <- prices$x[,2]
prices$max <- prices$x[,3]
prices$x <- NULL
colnames(prices) <- c("category","shop","currency","min","mean","max")

data <- prices[grep("*",prices[,1],ignore.case = TRUE),]
data <- data[order(data$category,data$min,data$currency),]

hline.after <- c(-1,0,which(diff(as.numeric(data$category)) > 0),nrow(data))

require(xtable)

write("\\documentclass[a4paper]{article} \\pagestyle{empty} \\usepackage[utf8]{inputenc} \\usepackage{longtable} \\usepackage[left=1cm, right=1cm, bottom=1cm,top=1cm]{geometry} \\begin{document}",file="food-prices.tex")
print(xtable(data,digits=5),type="latex",file="food-prices.tex",
      append=TRUE,tabular.environment = 'longtable',
      include.rownames=FALSE,floating=FALSE,
      hline.after = hline.after)
write("\\end{document}",file="food-prices.tex",append=TRUE)

system("pdflatex food-prices.tex && pdflatex food-prices.tex")

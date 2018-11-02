#!/usr/bin/env Rscript

#' @title save dataframe in a ledger format
#'
#' @param data dataframe that contains columns 'date', 'payee', 'to', 'from', 'currency'
#'
#' @param volumes boolean variable that indicates if volume comments
#'   should be included. If TRUE then data must contain 'volume'
#'   column
#'
#' @param tags booleans variable that indicates if transactions should
#'   be tagged. If TRUE then data must contain 'tags' column
#'
#' @return nothing
save_dataframe <- function(data, file = "dummy.ledger", volumes = FALSE, tags = FALSE) {
  s <- paste0(format(as.Date(data$date),format="%Y/%m/%d")," ",data$payee,"\n")

  if (tags) s <- paste0(s, "    ; ",data$tags,"\n")

  s <- paste0(s, "    ",data$to,"    ",data$value," ",data$currency)

  if (volumes) s <- paste0(s," ; ",data$volume)

  s <- paste0(s,"\n","    ",data$from,"\n")

  write(x=s,file=file)
}

set.seed(1)

# number of days
N <- 365*3

# add expenses entries
expenses <- data.frame("date" = seq(Sys.Date()-N+1,Sys.Date(),by=1),
                       "payee" = "payee",
                       "from" = "Assets",
                       "to" = sample(c("Expenses:Food","Expenses:Housing",
                                       "Expenses:Transport","Expenses:Entertainment"),
                                     N,TRUE),
                       "value" = round(runif(N,min=5,max=100),digits=2),
                       "currency" = sample(c("EUR","USD"),N,TRUE))

# add income entries
expenses <- rbind(expenses,
                  data.frame("date" = sample(seq(Sys.Date()-N+1,Sys.Date(),by=1),36),
                             "payee"= "payee",
                             "from" = "Income",
                             "to" = "Assets",
                             "value" = round(runif(36,min=900,max=1500),digits=2),
                             "currency" = "EUR"))

# order entries by date
expenses <- expenses[order(expenses$date),]

# add random tags here and there
expenses$tags <- ""
idx <- sample(1:nrow(expenses),nrow(expenses)/4) # where tags will be put
expenses$tags[idx] <- paste0(expenses$tags[idx],":tag1")
idx <- sample(1:nrow(expenses),nrow(expenses)/4) # where tags will be put
expenses$tags[idx] <- paste0(expenses$tags[idx],":tag2")
idx <- sample(1:nrow(expenses),nrow(expenses)/4) # where tags will be put
expenses$tags[idx] <- paste0(expenses$tags[idx],":tag3")
idx <- sample(1:nrow(expenses),nrow(expenses)/4) # where tags will be put
expenses$tags[idx] <- paste0(expenses$tags[idx],":tag4")

# add colon in the end of each tag
idx <- nchar(expenses$tags) != 0
expenses$tags[idx] <- paste0(expenses$tags[idx],":")

# save expenses entries
save_dataframe(data=expenses, file = "expenses.ledger", volume = FALSE, tags=TRUE)

set.seed(2)

# add food entries
food <- data.frame("date" = seq(Sys.Date()-N+1,Sys.Date(),by=1),
                   "payee" = sample(paste0("payee",1:10),N,TRUE),
                   "from" = "Assets",
                   "to" = sample(c("Food:Dairy","Food:Fruits","Food:Vegetables","Food:Snacks"),N,TRUE),
                   "value" = round(runif(N,min=5,max=100),digits=2),
                   "currency" = sample(c("EUR"),N,TRUE),
                   "volume" = paste0(round(runif(N,min=1,max=5),digits=2),sample(c("x","kg","l","qb"),N,TRUE)))

# save food entries
save_dataframe(data=food, file = "food.ledger", volume = TRUE, tags = FALSE)

# more realistic expenses (for plotting nice forecasts)
set.seed(3)

# dates for salary
N <- 365*10
d <- seq(Sys.Date()-N+1,Sys.Date(),by=30) # payday every 30 days
d <- d + sample(c(-2,-1,0,1,2),length(d),TRUE) # plus random changes

# we are getting paid about 1000 that increases 1% per year
# (also, exp modulated periodic)
expenses2 <- data.frame("date" = d,
                        "payee" = "payee",
                        "from" = "Income",
                        "to" = "Assets",
                        "value" = round((1+0.03/12)^(1:length(d))*runif(length(d),min=900,max=1100),digits=2),
                        "currency" = "X")

# we have housing expenses (we are lucky and it is constant)
d <- seq(Sys.Date()-N+1,Sys.Date(),by=30)
d <- d + sample(c(-1,0,1),length(d),TRUE)
expenses2 <- rbind(expenses2,
                   data.frame("date"=d,
                              "payee" = "payee",
                              "from" = "Assets",
                              "to" = "Expenses:Housing",
                              "value" = 500,
                              "currency" = "X"))

# we have some taxes that we pay every 90 days or so
d <- seq(Sys.Date()-N+1,Sys.Date(),by=90)
d <- d + sample(c(-1,0,1),length(d),TRUE)
expenses2 <- rbind(expenses2,
                   data.frame("date"=d,
                              "payee" = "payee",
                              "from" = "Assets",
                              "to" = "Expenses:Taxes",
                              "value" = round(runif(length(d),min=100,max=150),digits=2),
                              "currency" = "X"))

# and, of course, we need to eat something (say about every three days)
d <- sample(seq(Sys.Date()-N+1,Sys.Date(),by=1),N/3)
expenses2 <- rbind(expenses2,
                   data.frame("date"=d,
                              "payee" = "payee",
                              "from" = "Assets",
                              "to" = "Expenses:Food",
                              "value" = round(runif(length(d),min=10,max=50),digits=2),
                              "currency" = "X"))


# order entries by date
expenses2 <- expenses2[order(expenses2$date),]

# save expenses
save_dataframe(data=expenses2, file = "expenses-realistic.ledger", volume = FALSE, tags = FALSE)

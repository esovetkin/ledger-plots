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

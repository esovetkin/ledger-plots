#' @title parse file with food nutrition value characteristics
#'
#' @description parse file containing account characteristics
#'
parse.account.constrains.matrix <- function(filename)
{
  require("yaml")

  filename <- "~/bank/nutrition-values/food-nutritions.yaml"

  # raw data
  data <- yaml.load_file(filename)

  # account names
  accounts <- unique(names(data))

  # characteristics names
  characteristics <- unique(unlist(lapply(data, names)))

  # nutrition table (constrain matrix)
  res <- matrix(NA, ncol=length(characteristics), nrow=length(accounts))
  colnames(res) <- characteristics
  rownames(res) <- accounts

  # fill the table
  for (i in 1:length(data)) {
    res[names(data[i]),names(data[[i]])] <- unlist(data[[i]])
  }

  re <- "(.*) (.*)/(.*)"
  v <- gsub(re,"\\1",res)

  res <- sapply(v, function(x) eval(parse(text=x)))
  dim(res) <- dim(v)
  colnames(res) <- characteristics
  rownames(res) <- accounts
  res
}

#' @title match the interval values
#'
#' @param data raw strings read from file
#' 
match.interval.constrains <- function(data)
{
  re <- "\\(([.0-9]*),([.0-9]*)\\)[ ]?(.*)/(.*)"

  data.frame("lower"=as.numeric(gsub(re,"\\1",data)),
             "upper"=as.numeric(gsub(re,"\\2",data)),
             "unit"=gsub(re,"\\3",data),
             "period"=gsub(re,"\\4",data))
}


#' @title parse nutrition recommendation file
#'
#' @param filename name of the file containing the nutrition
#'   constrains
#'
#' 
parse.nutrition.values <- function(filename)
{
  require("yaml")

  ### temp
  filename <- "~/bank/nutrition-values/recommended-constrains.yaml"
  
  # raw data
  data <- unlist(yaml.load_file(filename))

  # constrains
  constr <- match.interval.constrains(data)

  # convert periods to a common value
  
  # index of rows to be duplicated for nutritions table
  idx <- rep(1:nrow(constr),
             as.numeric(apply(!is.na(constr[,c("lower","upper")]),1,all))+1)

  # generate the constraint vector
  rhs <- as.numeric(t(constr[,c("lower","upper")]))

  s <- matrix(c(rep(">=",nrow(constr)),rep("<=",nrow(constr))),ncol=2)
  s <- as.character(t(s))[!is.na(rhs)]

  rhs <- rhs[!is.na(rhs)]

  list("idx"=idx,"rhs"=rhs,"constr.dir"=s)
}

#' @title get price vector
#' 
get.prices <- function()
{
  data <- read.ledger(query="",options="-f ~/bank/food-ledger_2013.log -f ~/bank/food-ledger_2014.log -f ~/bank/food-ledger_2015.log -f ~/bank/food-ledger_2016.log -f ~/bank/food-ledger.log -X EUR")

  # parse transaction notes
  data <- parse.notes(data, c("1kg = 1000g","1kg = 1l", "1kg = 1000ml"))

  data <- data[data$Price.curr %in% "EUR/kg" ,]
  
  tree <- account.tree.depth(data$Category)

  res <- sapply(as.character(tree$Account), function(x)
  {
    data.frame("Account"=x,"Price"=mean(data[grep(x, data$Category),"Price"]),
               stringsAsFactors=FALSE)
  })
  res <- t(res)
  
  res
}

#' @title find optimal combination of accounts and volumes
#'
#' 
optimal.food <- function()
{
  require(lpSolve)

  # parse nutrition values for food
  constr <- parse.account.constrains.matrix("temp")

  # parse required limits
  v <- parse.nutrition.values("temp")

  # use prices as objective function
  obj <- get.prices()

  obj <- obj[rownames(obj) %in% rownames(constr),]
  constr <- constr[rownames(constr) %in% rownames(obj),]

  constr <- constr[order(rownames(constr)),]
  obj <- obj[order(rownames(obj)),]

  i <- which(is.na(obj[,"Price"]))
  
  obj <- obj[!rownames(obj) %in% names(i),]
  constr <- constr[!rownames(constr) %in% names(i),]
  
  obj <- obj[,"Price"]
  obj <- unlist(obj)
  
  constr <- constr[,v$idx]
  constr.dir <- v$constr.dir
  rhs <- v$rhs
  
  l <- lp("min", obj, t(constr), constr.dir, rhs)
}

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

  # remove rows that have no interval
  constr <- constr[!apply(is.na(constr[,c("lower","upper")]),1,all),]

  # convert periods to a common value

  # index of rows to be duplicated for nutritions table
  idx <- rep(rownames(constr),
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

#' @title fill missing values for the nutrition values
#'
#' Fill missing values by taking averages from the children of the
#' same node. Going from most bottom level to the most top level of
#' the account tree.
#'
#' @param data data.frame containing columns of values with possibly
#'   missing values
#'
#' @return vector with missing values replaces following the algorithm
#'   described above
fill.missing.nutritions <- function(data)
{
  tree <- account.tree.depth(rownames(data))

  # get the parent node
  tree$Parent <- gsub("(.*):[^:]+","\\1",tree$Account)

  for (d in sort(unique(tree$Depth),decreasing=TRUE)) {
    # get list of parent nodes with current depth
    accounts <- unique(tree[tree$Depth == d,"Parent"])

    # calculate averages of the missing values
    for (a in accounts) {
      data[grep(a,rownames(data)),] <- apply(
        data[grep(a,rownames(data)),,drop=FALSE],2,function(x)
        {
          na.idx <- is.na(x)

          if ( all(na.idx) )
            return(x)

          x[na.idx] <- mean(x[!na.idx])

          x
        })
    }
  }

  data
}

#' @title find optimal combination of accounts and volumes
#'
#'
optimal.food <- function()
{
  require(lpSolve)

  # parse nutrition values for food
  constr <- parse.account.constrains.matrix("temp")

  # fill missing nutrition value
  constr <- fill.missing.nutritions(constr)

  # parse required limits
  v <- parse.nutrition.values("temp")

  # use prices as objective function
  obj <- get.prices()

  obj <- obj[rownames(obj) %in% rownames(constr),,drop=FALSE]
  constr <- constr[rownames(constr) %in% rownames(obj),,drop=FALSE]
  constr <- constr[,colnames(constr) %in% v$idx,drop=FALSE]

  # make account sorted in the same way
  constr <- constr[order(rownames(constr)),,drop=FALSE]
  obj <- obj[order(rownames(obj)),,drop=FALSE]

  # remove accounts where no price is present
  i <- which(is.na(obj[,"Price"]))
  obj <- obj[!rownames(obj) %in% names(i),,drop=FALSE]
  constr <- constr[!rownames(constr) %in% names(i),,drop=FALSE]

  obj <- obj[,"Price"]
  obj <- unlist(obj)

  constr <- constr[,v$idx]
  constr.dir <- v$constr.dir
  rhs <- v$rhs

  l <- lp("min", obj, t(constr), constr.dir, rhs)
}

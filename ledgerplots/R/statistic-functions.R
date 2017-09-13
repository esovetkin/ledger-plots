#' @title Calculate 360 days average of the given vector
#'
#' @param x a numeric vector
#'
#' @return a numeric vector of the same size as vector x
#'
#' @export
yearly <- function(x) {
  if (length(x) < 365)
    return(rep(NA,length(x)))

  filter(x,rep(1,365),sides=1)
}

#' @title Calculate 30 days average of the given vector
#'
#' @description example of a functions can be used to be appled for
#'   the transaction vectors
#'
#' @param x a numeric vector
#'
#' @export
monthly <- function(x) {
  if (length(x) < 30)
    return(rep(NA,length(x)))

  filter(x,rep(1,30),sides=1)
}

#' @title Calculate 7 days sum of the fiven vector
#'
#' @param x a numeric vector
#'
#' @export
weekly <- function(x) {
  if (length(x) < 7)
    return(rep(NA,length(x)))

  filter(x,rep(1,7),sides=1)
}

#' @title Monthly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
yearly.price <- function(x) {
  if (length(x) < 365)
    return(rep(NA,length(x)))

  n <- filter(abs(x) > 0,rep(1,365),sides=1)
  n[! abs(n) > 0] <- 1
  filter(x,rep(1,365),sides=1)/n
}

#' @title Monthly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
monthly.price <- function(x) {
  if (length(x) < 30)
    return(rep(NA,length(x)))

  n <- filter(abs(x) > 0,rep(1,30),sides=1)
  n[! abs(n) > 0] <- 1
  filter(x,rep(1,30),sides=1)/n
}

#' @title Calculate 7 days average price
#'
#' @param x a numeric vector
#'
#' @export
weekly.price <- function(x) {
  if (length(x) < 7)
    return(rep(NA,length(x)))

  n <- filter(abs(x) > 0,rep(1,7),sides=1)
  n[! abs(n) > 0] <- 1
  filter(x,rep(1,7),sides=1)
}

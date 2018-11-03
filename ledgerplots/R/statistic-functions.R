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

  stats::filter(x,rep(1,365),sides=1)
}

#' @title Calculate quartely averages
#'
#' @param x a numeric vector
#'
#' @return a numeric vector of the same size as vector x
#'
#' @export
quarterly <- function(x)
{
  if (length(x) < 90)
    return(rep(NA,length(x)))

  stats::filter(x,rep(1,90),sides=1)
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

  stats::filter(x,rep(1,30),sides=1)
}

#' @title Calculate 7 days sum of the fiven vector
#'
#' @param x a numeric vector
#'
#' @export
weekly <- function(x) {
  if (length(x) < 7)
    return(rep(NA,length(x)))

  stats::filter(x,rep(1,7),sides=1)
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
  stats::filter(x,rep(1,365),sides=1)/n
}

#' @title Quarterly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
quarterly.price <- function(x) {
  if (length(x) < 90)
    return(rep(NA,length(x)))

  n <- filter(abs(x) > 0,rep(1,90),sides=1)
  n[! abs(n) > 0] <- 1
  stats::filter(x,rep(1,90),sides=1)/n
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
  stats::filter(x,rep(1,30),sides=1)/n
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
  stats::filter(x,rep(1,7),sides=1)
}

#' @title linear regression forecast
#'
#' @param using_last number of days to use in the forecast
#'
#' @param forecast_for number of days to forecast
#'
#' @export
lm_forecast <- function(x, using_last = 365, forecast_for = 365) {
  i <- tail(1:length(x),n=using_last)

  newdata <- data.frame("i"=c(tail(i,n=using_last),(length(x)+1):(length(x)+forecast_for)))

  res <- predict(lm(tail(x,n=using_last)~i),newdata=newdata)

  c(rep(NA,length(x)-using_last-1),res)
}

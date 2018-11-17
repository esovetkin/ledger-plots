#' @title Calculate 360 days average of the given vector
#'
#' @param x a numeric vector
#'
#' @return a numeric vector of the same size as vector x
#'
#' @export
yearly <- function(x) .period_average(x,365)

#' @title Calculate quartely averages
#'
#' @param x a numeric vector
#'
#' @return a numeric vector of the same size as vector x
#'
#' @export
quarterly <- function(x) .period_average(x,90)

#' @title Calculate 30 days average of the given vector
#'
#' @param x a numeric vector
#'
#' @export
monthly <- function(x) .period_average(x,30)

#' @title Calculate 7 days sum of the fiven vector
#'
#' @param x a numeric vector
#'
#' @export
weekly <- function(x) .period_average(x,7)

#' @title Monthly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
yearly.price <- function(x) .period_price(x=x,days=365)

#' @title Quarterly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
quarterly.price <- function(x) .period_price(x=x,days=90)

#' @title Monthly average price
#'
#' @description calculate monthly average price
#'
#' @param x a numeric vector
#'
#' @export
monthly.price <- function(x) .period_price(x=x,days=30)

#' @title Calculate 7 days average price
#'
#' @param x a numeric vector
#'
#' @export
weekly.price <- function(x) .period_price(x=x,days=7)

#' @title compute a rolling average
#'
#' @param x a numeric vector
#'
#' @param days number of days in the window
#'
#' @return a numeric vector
#'
#' @export
.period_average <- function(x, days) {
  if (length(x) < days)
    return(rep(NA,length(x)))

  stats::filter(x,rep(1,days),sides=1)
}

#' @title compute a rolling price
#'
#' @param x a numeric vector
#'
#' @param days number of days in the window
#'
#' @return a numeric vector
#'
#' @export
.period_price <- function(x,days) {
  if (length(x) < days)
    return(rep(NA,length(x)))

  # number of price observations in a window
  n <- stats::filter(abs(x) > 0,rep(1,days),sides=1)
  n[! abs(n) > 0] <- 1

  # compute average price in a window
  stats::filter(x,rep(1,days),sides=1)/n
}

#' @title linear regression forecast
#'
#' @param x a numeric vector
#'
#' @param using_last number of days to use in the forecast
#'
#' @param forecast_for number of days to forecast
#'
#' @return vector, regression line values
#'
#' @export
lm_forecast <- function(x, using_last = 365, forecast_for = 365) {
  i <- tail(1:length(x),n=using_last)

  newdata <- data.frame("i"=c(tail(i,n=using_last),(length(x)+1):(length(x)+forecast_for)))

  res <- predict(lm(tail(x,n=using_last)~i),newdata=newdata)

  c(rep(NA,length(x)-using_last),res)
}

#' @title forecast values adjust
#'
#' adjust forecast values with known future transaction amounts
#'
#' @param dates future dates of transactions
#'
#' @param values values of transactions
#'
#' @param forecast vector of the forecasted values
#'
#' @param FUN function to be aplied to the adjusted values
#'
#' @return vector adjusted forecast vector
#'
#' @export
adjust_forecast <- function(forecast,dates,values,FUN="cumsum") {
  # compute position in the forecast vector
  dates_idx <- as.numeric(difftime(as.Date(dates),Sys.Date(),units="days"))

  # select only values with positive position
  values <- values[(dates_idx > 0) & (dates_idx <= length(forecast))]
  dates_idx <- dates_idx[(dates_idx > 0) & (dates_idx <= length(forecast))]

  # get adjusted vector
  adj <- rep(0,length(forecast))
  adj[dates_idx] <- values
  adj <- FUN(adj)

  return(forecast+adj)
}

#' @title Compute all-time-running daily average
#'
#' @param x a numeric vector
#'
#' @param ignore.days number of first days to ignore
#'
#' @return a numeric vector of the same size
#'
#' @export
alltime <- function(x, ignore.days = 30) {
  if (length(x) < ignore.days)
    return(rep(NA,length(x)))

  res <- cumsum(x)/(1:length(x))
  res[1:ignore.days] <- NA

  return(res)
}

#' @title Compute all-time-running daily average price
#'
#' @param x a numeric vector
#'
#' @param ignore.days number of first days to ignore
#'
#' @return a numeric vector of the same size
#'
#' @export
alltime.price <- function(x, ignore.days = 30) {
  if (length(x) < ignore.days)
    return(rep(NA,length(x)))

  # number of price observations in a window
  n <- cumsum(abs(x) > 0)
  n[! abs(n) > 0] <- 1

  # compute average price
  cumsum(x)/n
}

#' @title ssa forecast
#'
#' @param x a numeric vector
#'
#' @param group vector indicating which components to choose
#'
#' @param forecast_for number of days to forecast
#'
#' @export
ssa_forecast <- function(x,group,forecast_for = 365) {
  tryCatch({
    p <- predict(Rssa::ssa(x),groups=list(group),
                 method="recurrent",len=forecast_for)

    c(rep(NA,length(x)),p)
  }, error = function(e) rep(NA,length(x)))
}

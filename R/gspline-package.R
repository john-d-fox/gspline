

#' Canadian Monthly Unemployment Rate
#' 
#' Monthly unemployment data for Canada for the period 1995-01-01 through
#' 2019-02-01
#' 
#' 
#' @name Unemployment
#' @docType data
#' @format A data frame with 290 observations on the following 2 variables.
#' \describe{ \item{date}{a \code{"Date"} object.}
#' 
#' \item{unemployment}{Percentage unemployed.} }
#' @source Downloaded from \url{https://research.stlouisfed.org/} on
#' 2019-03-18.
#' @keywords datasets
#' @examples
#' 
#'  plot(unemployment ~ date, type="l", lwd=2, data=Unemployment)
#' 
NULL




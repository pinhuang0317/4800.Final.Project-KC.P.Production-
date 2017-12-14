#' Expected Value Calculation
#'
#' This function is designed to find the expected value for g(x) or g(x, y) given a function g.
#'
#' @param g the function that we are looking for expected value
#' @param rv given random variables
#'
#' @return the expected value of the given function
#' @export
#'
#' @examples
#'
#' betaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 2*x, 0)}
#' x1 = data.frame(oneDsample(f = betaPDF, N=10000, lb = 0, ub = 1))
#' g1 = function(x){x^2}
#' Evalue(g1,x1)
#'
#' jointPDF <- function(x){
#' x1 = x[1]
#' x2 = x[2]
#' ifelse(0<x1 & x1 <1 & 0<x2 & x2<1 & 0<x1+x2 & x1+x2<1, 24*x1*x2, 0)}
#' xy = data.frame(twoDsample(f = jointPDF, N=10000, lbx=0, ubx=1, lby=0, uby=1))
#' g2 = function(x,y){x^2+y}
#' Evalue(g2,xy)

Evalue <-function(g,rv){
  len = length(rv)
  if (len == 2){
    meanxy = mean(g(rv$x,rv$y))
    return(meanxy)
  }
  else if (len == 1){
    meanx = mean(g(rv))
    return(meanx)
  }
  else {
    stop("Error: invalid variables format")
  }
}

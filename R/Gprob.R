#' Conditional Probability Calculation
#'
#' This function is designed to calculate the probabilities realted to X or X and Y.
#'
#' @param cd the given conditions
#' @param data the samples obtained from rejection sampling
#'
#' @return the value of probability realted to X or X and Y
#' @export
#'
#' @examples
#'
#' f <- function(x) {ifelse(0 < x & x < 1, 4*x^3, 0)}
#' x1 = oneDsample(f,10000)
#' c1 = function(x){x<0.4}
#' Gprob(c1,x1)
#'
#' f <- function(x){
#' x1 = x[1]
#' x2 = x[2]
#' ifelse(x2>0, 1/pi/(1+x1^2) * 0.05*exp(-0.05*x2), 0)}
#' x2 = twoDsample(f = f, N=10000)
#' c2 = function(x,y){
#'  x<0.5 & y<0.2}
#' Gprob(c2,x2)
#'

Gprob <-function(cd,data){
  len = length(data)
  if (len == 2){
    meanxy = mean(cd(data$x,data$y))
    return(meanxy)
  }
  else if (len == 1){
    meanx = mean(cd(data))
    return(meanx)
  }
  else {
    stop("Error: invalid data format")
  }
}


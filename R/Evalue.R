#' what is the function does: Expected Value
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
}

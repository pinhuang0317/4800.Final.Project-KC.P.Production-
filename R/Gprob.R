#' what is the function does: Calculate Probability
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
}

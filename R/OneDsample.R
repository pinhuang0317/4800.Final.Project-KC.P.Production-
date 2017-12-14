#' what is the function does: Single Variable Rejection Sampling
#'
#' This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#'

ImportS: ggplot2

#'
#' @param f the pdf that we are sampling from
#' @param N the number of attempted samples.
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f
#'
#' @return A vector containing samples from pdf
#' @export
#'
#' @examples
#'
#'
#' f <- function(x) {ifelse(0 < x & x < 1, 4*x^3, 0)}
#' a <- oneDsample(f,10000, 0, 1)
#' ggplot(a, aes(x)) + geom_density() + stat_function(fun = f, color = "red")

oneDsample <- function(f, N, lb, ub, discrete = FALSE) {
  if (discrete == TRUE){
    warning("We cannot test whether a discrete function is a pdf")
  }
  if (discrete == FALSE){
    if (abs(integrate(f, -Inf, Inf)$val - 1) > 0.001) {
      stop("Error: not a pdf. The area under the function you given should be 1")
    }
  }
  maxf <- max(f(runif(100000,lb,ub)))
  ones = c()
  n = 0
  while (n < N) {
    one <-runif(1,lb,ub)
    if (runif(1, 0, maxf) < f(one)){
      ones = c(ones, one)
      n = n + 1
    }
  }
  return(data.frame(x=ones))
}



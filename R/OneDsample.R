#' what is the function does: Single Variable Rejection Sampling
#'
#' This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#'
#' @param f the pdf that we are sampling from
#' @param N the nimber of attempted samples.
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f
#'
#' @return A vector containing samples from pdf
#' @export
#'
#' @examples
#'
#' betaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 2*x, 0)}
#' oneDsample(f = betaPDF, N=10000, lb = 0, ub = 1)
#'
#' hist(oneDsample(f = betaPDF, N=1000000, lb = 0, ub = 1))
#'


oneDsample <- function(f, N, lb, ub) {
  if (abs(integrate(f, -Inf, Inf)$val - 1) > 0.001) {
    stop("Error: not a pdf. The area under the function you given should be 1")
  }
  else{
    ones <- runif(N, lb, ub)
    maxf <- max(f(runif(100000,lb,ub)))
    unis <- runif(N, 0, maxf)
    ones[unis < f(ones)]
  }
}



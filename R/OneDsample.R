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
#' a <- oneDsample(f,20000, 0, 1)
#' ggplot(a,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#'
#' f <- function(x) {1/pi/(1+x^2)}
#' a <- oneDsample(f,20000)
#' ggplot(a,aes(x)) + geom_density() + stat_function(fun = f, color = "red")

oneDsample <- function(f, N, lb = -Inf, ub = Inf, discrete = FALSE) {
  bdtest <- runif(1000000,-50,50)
  if (f(-50) == 0 & f(50) == 0){
    lb = min(bdtest[which(f(bdtest)>0)])
    ub = max(bdtest[which(f(bdtest)>0)])
  }
  if (discrete == TRUE){
    warning("We cannot test whether a discrete function is a pdf")
  }
  if (discrete == FALSE){
    if (abs(integrate(f, -Inf, Inf)$val - 1) > 0.001) {
      stop("Error: not a pdf. The area under the function you given should be 1")
    }
  }
  if (lb != -Inf & ub != Inf){
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
  else {
    x <- runif(200000,-10000,10000)
    maxf <- max(f(x))
    mu=x[which(f(x)==maxf)]
    sd = 2/maxf
    C = maxf/dnorm(mu,mu,sd)
    ones = c()
    n = 0
    while (n < N) {
      one = rnorm(1, mu, sd)
      if (runif(1, 0, C * dnorm(one,mu,sd)) < f(one)){
        ones = c(ones, one)
        n = n + 1
      }
    }
    return(data.frame(x=ones))
  }
}





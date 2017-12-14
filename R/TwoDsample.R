#' Conditional variable Rejection Sampling
#'
#' This function implements conditional variabel rejection sampling for rvs with bounded support x,y and which have have bounded pdf.
#'

ImportS: ggplot2
ImportS: cubature
ImportS: MASS

#'
#' @param f the joint pdf that we are sampling from
#' @param N the number of attempted samples.
#' @param lbx lower bound of support x of f
#' @param ubx upper bound of support x of f
#' @param lby upper bound of support y of f
#' @param uby upper bound of support y of f
#'
#' @return A vector containing samples from pdf
#'
#' @export
#' @examples
#'
#' jointPFF <- function(x){
#' x1 = x[1]
#' x2 = x[2]
#' ifelse(0<x1 & x1<1 & 0<x2 & x2<1 & 0<x1+x2 & x1+x2<1, 24*x1*x2, 0)}
#'
#' f <- function(x){
#' x1 = x[1]
#' x2 = x[2]
#' ifelse(x2>0, 1/pi/(1+x1^2) * 0.05*exp(-0.05*x2), 0)}
#'
#' a <- twoDsample(f = f, N=10000)
#' ggplot(a, aes(x, y)) +  geom_density_2d()

twoDsample <- function(f, N, lbx=-5000, ubx=5000, lby=-5000, uby=5000) {
  library(MASS)
  library(cubature)
  if (abs(adaptIntegrate(f, c(lbx, lby), c(ubx, uby), maxEval=10000)$integral - 1) > 0.001) {
    stop("Error: Bound is missing/wrong or the function is not a pdf. The area under the function you given should be 1")
  }
  if (lbx != -5000 & ubx != 5000 & lby != -5000 & uby != 5000){
    maxf <- max(replicate(100000,f(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
    twos = c()
    n = 0
    while (n < N) {
      two <- c(runif(1,lbx,ubx),runif(1,lby,uby))
      if (runif(1, 0, maxf) < f(two)){
        twos = c(twos, two)
        n = n+1
      }
    }
    data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))])
  }
  else{
    dmvnorm = function(x,mu,sig){
      x1 = x[1]
      x2 = x[2]
      mu1 = mu[1]
      mu2 = mu[2]
      sig1 = sig[1]
      sig2 = sig[2]
      exp(-1/2*((x1-mu1)^2/sig1^2 - 2*(x1-mu1)*(x2-mu2)/sig1/sig2 + (x2-mu2)^2/sig2^2))/(2*pi*sig1*sig2)
    }
    op = optim(c((ubx+lbx)/2,(uby+lby)/2), f, control = list(fnscale = -1))
    maxf = op$value
    mu = c(op$par)
    sd = 2/maxf
    C = maxf/dmvnorm(c(mu[1],mu[2]),c(mu[1],mu[2]),c(sd,sd))
    twos = c()
    n = 0
    while (n < N) {
      two = mvrnorm(1, mu, matrix(c(sd,0,0,sd),2,2))
      if (runif(1, 0, C * dmvnorm(two,mu,c(sd,sd))) < f(two)){
        twos = c(twos, two)
        n = n + 1
      }
    }
    return(data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))]))
  }
}


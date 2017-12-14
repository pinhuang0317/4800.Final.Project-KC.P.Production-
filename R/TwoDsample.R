
twoDsample <- function(fj, N, lbx, ubx, lby, uby) {
  two <- replicate(N,c(runif(1,lbx,ubx),runif(1,lby,uby)))
  maxfj <- max(replicate(100000,fj(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
  unis <- runif(N, 0, maxfj)
  f1 = -1
  f3 = 0
  twos = c()
  for (i in 1:N) {
    f1 = f1 + 2
    f2 = f1 + 1
    f3 = f3 + 1
    if (unis[f3]<fj(c(two[f1],two[f2]))){
      twos = c(twos, c(two[f1],two[f2]))
    }
  }
  data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))])
}

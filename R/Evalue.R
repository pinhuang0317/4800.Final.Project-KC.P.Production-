
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

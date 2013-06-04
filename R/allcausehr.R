allcausehr <-
function(t, all.bhr, eta.ij){
  res <- 0
  p <- length(eta.ij)
  for(hi in 1:p){
    res <- res + hr(all.bhr[[hi]], t, eta.ij[hi])
  }
  return(res)}

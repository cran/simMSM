allcausehr <- Vectorize(function(t, all.bhr, eta.ij, x.i){
    res <- 0
    p <- length(eta.ij)
    for(hi in 1:p){
        res <- res + hr(bhr = all.bhr[[hi]], t = t, eta.ij = eta.ij[[hi]], x.i = x.i)
    }
    return(res)}, "t")
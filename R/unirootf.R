unirootf <- function(exit, u, entry, all.bhr, eta.ij, x.i){
    return(cumallcausehr(entry, exit, all.bhr, eta.ij = eta.ij, x.i = x.i) + log(1-u))}
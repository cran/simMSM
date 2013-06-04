f.for.uniroot <-
function(exit, u, entry, all.bhr, eta.ij=eta.ij){
  return(cumallcausehr(entry, exit, all.bhr, eta.ij=eta.ij)+log(1-u))}

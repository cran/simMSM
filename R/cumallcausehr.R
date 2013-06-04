cumallcausehr <-
function(entry, exit, all.bhr, eta.ij=eta.ij){
  ## print("integrate")
  return(integrate(allcausehr, lower=entry, upper=exit, all.bhr, eta.ij=eta.ij, 
                   subdivisions=10000)$value)}

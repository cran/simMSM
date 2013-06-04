sim.to <-
function(entry.ij, from.ij, all.to.all.bhr.all.beta, eta.ij=eta.ij){
  exit.ij <- sim.exit(entry.ij, all.to.all.bhr.all.beta[[from.ij]]$all.bhr, 
                      eta.ij=eta.ij)$new.exit
  hr.at.exit.ij <- rep(NA, length(eta.ij))
  for(hi in all.to.all.bhr.all.beta[[from.ij]]$all.to){
    hr.at.exit.ij[hi] <- hr(all.to.all.bhr.all.beta[[from.ij]]$all.bhr[[hi]], 
                            exit.ij, eta.ij[hi])
  }
  hr.at.exit.ij <- hr.at.exit.ij[!is.na(hr.at.exit.ij)]
  if(length(hr.at.exit.ij) > 1.5){
    probs <- hr.at.exit.ij/sum(hr.at.exit.ij)
    to.ij <- sample(all.to.all.bhr.all.beta[[from.ij]]$all.to, size=1, 
                    prob=probs)
  }else{
    to.ij <- as.numeric(all.to.all.bhr.all.beta[[from.ij]]$all.to)
  }
  return(list(entry.ij = entry.ij, exit.ij = exit.ij, from.ij = from.ij, 
              to.ij = to.ij))}

sim.exit <-
function(entry, all.bhr, eta.ij=eta.ij){
  u <- runif(1)
  ## print("uniroot")
  new.exit <- uniroot(f.for.uniroot, interval=c(entry, entry*1+100), 
                      u, entry, all.bhr, eta.ij=eta.ij)$root
  ## print(paste("new.exit: ", new.exit))
  return(list(new.exit = new.exit, u = u))}

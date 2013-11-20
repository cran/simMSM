sim.single.history <-
function(first.entry=0, first.from, max.time,
                               all.to.all.bhr.all.beta, x.i){
  current.exit <- current.entry <- first.entry
  current.to <- current.from <- first.from
  history.i <- NULL
  p <- length(x.i)
  while((current.entry < max.time) & (!is.null(all.to.all.bhr.all.beta[[current.from]]$all.to))){
    f.x <- all.to.all.bhr.all.beta[[current.from]]$all.beta
    eta.ij <- rep(0, length(f.x))
    for(oi in 1:length(f.x)){
      for(ii in 1:length(f.x[[oi]])){
        f.now <- f.x[[oi]][[ii]]
        eta.ij[oi] <- eta.ij[oi] + f.now(x.i[ii])
      }
    }
    current.sim <- sim.to(current.entry, current.from, all.to.all.bhr.all.beta,
                          eta.ij=eta.ij)
    history.i <- rbind(history.i,
                       c(current.sim$entry.ij, current.sim$exit.ij,
                         current.sim$from.ij, current.sim$to.ij))
    current.entry <- current.sim$exit.ij
    current.from <- current.sim$to.ij}
  return(history.i)}

simsinglehistory <- function(first.entry = 0, first.from, max.time, change.times, mpl, x.i, diagnostics){
  current.exit <- current.entry <- first.entry
  current.to <- current.from <- first.from
  history.i <- NULL
  p <- length(x.i)
  while((current.entry < max.time) & (!is.null(mpl[[current.from]]$all.to))){
    eta.ij <- vector("list", length(mpl[[current.from]]$eta))
    for(j in 1:length(eta.ij)){
      eta.ij[[j]] <- mpl[[current.from]]$eta[[j]]}
    current.sim <- simto(current.entry, current.from, mpl,
                         eta.ij = eta.ij, x.i = x.i, 
                         diagnostics = diagnostics, max.time = max.time)
    delta.ij <- 1
    current.entry <- current.sim$exit.ij
    current.from <- current.sim$to.ij
    if(!is.null(change.times)){
      for(cti in 1:length(change.times)){
        if((current.sim$entry.ij < change.times[cti]) & (current.sim$exit.ij > change.times[cti])){
          current.sim$exit.ij <- change.times[cti]
          delta.ij <- 0
          current.entry <- change.times[cti]
          current.from <- current.sim$from.ij
        }
      }
    }
    history.i <- rbind(history.i,
                       c(current.sim$entry.ij, current.sim$exit.ij,
                         current.sim$from.ij, current.sim$to.ij, delta.ij))
  }
  return(history.i)}
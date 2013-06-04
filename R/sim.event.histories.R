sim.event.histories <-
function(n, all.to.all.bhr.all.beta, max.time=10){
  hf <- function(x, k){return(x[[k]])}
  all.possible.from.states <- as.numeric(do.call(c, lapply(all.to.all.bhr.all.beta, 
                                                           FUN=hf, k=1)))
  all.first.from <- sample(all.possible.from.states, size=n, replace=T)
  p <- length(all.to.all.bhr.all.beta[[1]]$all.beta[[1]])
  all.x <- runif(n=n*p, min = -1, max = 1)
  all.x <- matrix(nrow=n, ncol=p, data=all.x)
  histories <- NULL
  for(i in 1:n){
    history.i <- sim.single.history(first.entry=0, first.from=all.first.from[i], 
                                    max.time, all.to.all.bhr.all.beta, 
                                    x.i = all.x[i, ])
    history.i <- cbind(history.i, rep(i, nrow(history.i)))
    for(x.index in 1:p){
      history.i <- cbind(history.i, rep(all.x[i, x.index], nrow(history.i)))      
    }
    histories <- rbind(histories, history.i); rm(history.i)
  }
  histories.as.list <- list("id" = histories[, 5], "entry" = histories[, 1], 
                            "exit" = histories[, 2], "from" = histories[, 3], 
                            "to" = histories[, 4])
  for(x.index in 1:p){
    histories.as.list[[5+x.index]] <- histories[, 5+x.index]
    names(histories.as.list)[5+x.index] <- paste("x", x.index, sep="")}
  histories <- data.frame(histories.as.list); rm(histories.as.list)
  return(histories)}

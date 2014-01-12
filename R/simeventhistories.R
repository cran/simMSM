simeventhistories <- function(n, mpl, max.time, change.times = NULL, X, diagnostics = FALSE, states.at.origin = NULL){
  if(is.null(states.at.origin)){
    hf <- function(x, k){
      if(!is.null(x$all.to)){
        return(x[[k]])
      }
    }
    all.possible.from.states <- as.numeric(do.call(c, lapply(mpl, FUN = hf, k = 1)))
    all.first.from <- sample(all.possible.from.states, size = n, replace = TRUE)
  }else{
    all.first.from <- sample(states.at.origin, size = n, replace = TRUE)
  }
  p <- ncol(X)
  histories <- NULL
  for(i in 1:n){
    history.i <- simsinglehistory(first.entry = 0,
                                  first.from = all.first.from[i],
                                  max.time, change.times, mpl, x.i = X[i, ],
                                  diagnostics)
    if(!is.null(nrow(history.i))){
      history.i <- cbind(history.i, rep(i, 1))
      for(x.index in 1:p){
        history.i <- cbind(history.i, rep(X[i, x.index], 1))
      }
    }else{
      history.i <- cbind(history.i, rep(i, nrow(history.i)))
      for(x.index in 1:p){
        history.i <- cbind(history.i, rep(X[i, x.index], nrow(history.i)))
      }
    }
    histories <- rbind(histories, history.i)
    rm(history.i)
  }
  ## hier noch delta:
  histories.as.list <- list("id" = histories[, 6], "entry" = histories[, 1],
                            "exit" = histories[, 2], "from" = histories[, 3],
                            "to" = histories[, 4], "delta" = histories[, 5])
  ## hier dann 5 durch 6 erstetzen?!
  for(x.index in 1:p){
    histories.as.list[[6 + x.index]] <- histories[, 6 + x.index]
    names(histories.as.list)[6 + x.index] <- colnames(X)[x.index]}
  histories <- data.frame(histories.as.list)
  rm(histories.as.list)
  return(histories)}
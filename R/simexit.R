simexit <- function(entry, all.bhr, eta.ij, x.i, diagnostics, max.time){
  result <- NULL
  while(is.null(result)){
    u <- runif(1)
    if(diagnostics){
      try(result <- simexitinner(u = u, entry = entry, all.bhr = all.bhr, eta.ij = eta.ij, x.i = x.i, max.time = max.time), 
        silent = FALSE)
      if(is.null(result)){
        cat(paste("Error for: entry=", round(entry, 3), "; u=", round(u, 3),"; x.i=(", paste(round(x.i, 3), collapse=","), ")\n", sep = ""))
      }
    }
    if(!diagnostics){
      try(result <- simexitinner(u = u, entry = entry, all.bhr = all.bhr, eta.ij = eta.ij, x.i = x.i, max.time = max.time), 
          silent = TRUE)
    }
  }
  return(result)
}
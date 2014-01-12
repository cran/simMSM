simexitinner <- function(u, entry, all.bhr, eta.ij, x.i, max.time){
  new.exit <- uniroot(unirootf, interval = c(entry, max.time * 1e4),
                      u = u, entry = entry, all.bhr = all.bhr, eta.ij = eta.ij, x.i = x.i)$root
  return(list(new.exit = new.exit, u = u))}
# if(round(x.i[1], 2) == -1.72){
#   plot.x <- seq(0, 100 + entry, length = 200)
#   plot.y <- rep(0, length(plot.x))
#   for(plot.i in 1:length(plot.x)){
#     plot.y[plot.i] <- unirootf(exit = plot.x[plot.i], u = u, entry = entry, all.bhr = all.bhr, eta.ij = eta.ij, x.i = x.i) 
#   }
#   plot(plot.x, plot.y, type = "l", main = round(x.i[1], 3))
#   abline(h = 0)
#   abline(v = entry)
#   Sys.sleep(10)
# }
#' @export
plot.wafer = function(x, y=NULL, direction="Forward", ...) {
  op = par(mar=c(3,3,2,1),  mgp=c(2,0.4,0), tck=-.01,
           cex.axis=0.9, las=1, mfrow=c(2,3))
  on.exit(par(op))
  x = x[x$direction == direction, ]
  
  for(i in 1:6){
    plot(x[, i+5],  panel.first = grid(), 
         ylab = bquote(theta[.(i)]), 
         pch=21, bg=x$id, xlab="", axes=FALSE, ...)
    axis(1, labels = FALSE, tick = FALSE)
    axis(2)
    }
}

#' @importFrom graphics grid hist legend lines mtext par plot
#' @export
hist.wafer = function(x, direction="Forward",...) {
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1, 
           mfrow=c(2, 3))
  on.exit(op)
  x = x[x$direction == direction, ]
  
  for (i in 1:6){
    hist(x[,i+5], 
         panel.first = grid(), 
         xlab = bquote(theta[.(i)]), main = "", col="steelblue")
  }
  par(mfrow = c(1, 1))
  mtext(direction, NORTH <- 3, line = 0.7, cex = 1)
  readline("Press enter for the next plot")  
  
  hist(x$cost, panel.first = grid(), xlab = "Cost", main = direction, col="steelblue")
  readline("Press enter for the next plot") 
  
}

#' @export
plot.wafer = function(x, y=NULL, direction="Forward", ...) {
  op = par(mar=c(3,3,2,1),  mgp=c(2,0.4,0), tck=-.01,
           cex.axis=0.9, las=1, mfrow=c(2,3))
  on.exit(par(op))
  x = x[x$direction == direction, ]
  
  plot(x$X1, ylab="X1", pch=21, bg=x$id, ...)
  plot(x$X2, ylab="X2", pch=21, bg=x$id, ...)
  plot(x$X3, ylab="X3", pch=21, bg=x$id, ...)
  plot(x$X4, ylab="X4", pch=21, bg=x$id, ...)
  plot(x$X5, ylab="X5", pch=21, bg=x$id, ...)
  plot(x$X6, ylab="X6", pch=21, bg=x$id, ...)
}

#' @importFrom graphics grid hist legend lines mtext par plot
#' @export
hist.wafer = function(x, ...) {
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  fitted = x
  par(mfrow = c(2, 3))
  for (i in 1:6){
    hist(fitted[fitted$direction=="Forward",][,i+5], panel.first = grid(), xlab = bquote(theta[.(i)]), main = "")
  }
  par(mfrow = c(1, 1))
  mtext("Forward", NORTH <- 3, line = 0.7, cex = 1)
  readline("Press enter for the next plot")  
  
  hist(fitted[fitted$direction=="Forward",]$cost, panel.first = grid(), xlab = "cost", main = "Forward")
  readline("Press enter for the next plot") 
  
  par(mfrow = c(2, 3))
  for (i in 1:6){
    hist(fitted[fitted$direction=="Backward",][,i+5], panel.first = grid(), xlab = bquote(theta[.(i)]), main = "")
  }
  par(mfrow = c(1, 1))
  mtext("Backward", NORTH <- 3, line = 0.7, cex = 1)
  readline("Press enter for the next plot") 
  
  hist(fitted[fitted$direction=="Backward",]$cost, panel.first = grid(),xlab = "cost",main = "Backward")
}

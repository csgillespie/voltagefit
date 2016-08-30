#' @export
plot.wafer = function(x, y=NULL,  cost=FALSE, ...) {
  op = par(mar=c(3,3,2,1),  mgp=c(2,0.4,0), tck=-.01,
           cex.axis=0.9, las=1, mfrow=c(1, 1))
  on.exit(par(op))
  #x = x[x$direction == direction, ]
  
  if(cost) {
    x_values = as.numeric(factor(x$id))
    y = x$cost
    plot(x_values, y, panel.first = grid(), 
         ylab = "Cost", 
         pch=21, bg=factor(x$direction), xlab="", axes=FALSE, ...)
    axis(1, labels = FALSE, tick = FALSE)
    axis(2) 
  } else {
    par(mfrow=c(2, 3))
    par_loc = get_par_loc(x)
    for(i in seq_along(par_loc)){
      x_values = as.numeric(factor(x$id))
      y = x[,par_loc[i]]
      plot(x_values, y,  panel.first = grid(), 
           ylab = bquote(theta[.(i)]), 
           pch=21, bg=factor(x$direction), xlab="", axes=FALSE, ...)
      axis(1, labels = FALSE, tick = FALSE)
      axis(2)
    }
  }
  invisible(NULL)
}

#' @importFrom graphics grid hist legend lines mtext par plot
#' @export
hist.wafer = function(x, direction="Forward", cost=FALSE, ...) {
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, 
           cex.axis = 0.9, las = 1,  mfrow=c(1, 1))
  on.exit(op)
  x = x[x$direction == direction, ]
  
  if(cost) {
    hist(x$cost, panel.first = grid(), xlab = "Cost", 
         main = direction, col="steelblue", ...)
  } else {
    par(mfrow = c(2, 3),oma=c(0,0,2,0))
    par_loc = get_par_loc(x)
    for (i in seq_along(par_loc)){
      hist(x[, par_loc[i]], 
           panel.first = grid(), 
           xlab = bquote(theta[.(i)]), 
           main = "", 
           col="steelblue", ...)
    }
    title(direction, outer=TRUE)
  }
  invisible(NULL)
}

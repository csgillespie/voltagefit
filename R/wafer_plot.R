#' Plot fitted voltage curve and wafer curves
#'
#' Plot fitted voltage curve and the wafer curves the fit was 
#' generated with.
#' 
#' @param x fitted wafer data as from \code{fit_wafer}.
#' @param plotit Default \code{TRUE}. 
#' 
#' @importFrom ggplot2 aes geom_line ggplot scale_y_log10 labs
#' @export
plot_fit = function(x, plotit = TRUE) {
  wafer = attr(x,"wafer_back_forward")
  curve = attr(x,"dev_curve")
  par_loc = get_par_loc(x)
  
  ## TODO: Growing a vector here. 
  ## Terrible code. Redo
  dd = NULL
  for(i in 1:nrow(x)) {
    dd_tmp = voltagefit:::get_curve(x[i, par_loc], dev_curve = attr(x, "dev_curve"))
    dd_tmp$id = x[i, "id"]
    dd_tmp$direction = x[i,"direction"]
    dd = rbind(dd, dd_tmp)
  }

  ## Remove bad devices
  wafer = trans_device(wafer)
  if(plotit) {
    g = ggplot(wafer) + geom_line(aes(VG, abs(ID), group=name), alpha=0.1) + 
      scale_y_log10() + labs(title = paste("wafer:", wafer$wafer_id[1]))
    g = g + geom_line(data=dd,  aes(VG, ID), col="steelblue") + 
      facet_wrap(~direction)
    print(g)
  }
  dd
}

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
  } else{
    par(mfrow=c(4,2))
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

#' @importFrom graphics grid hist legend lines mtext par plot title
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
    par(mfrow = c(4, 2),oma=c(0,0,2,0))
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

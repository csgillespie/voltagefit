#' Plot histograms of fitted wafer curve parameters and cost function
#'
#' Function to plot histograms of fitted wafer curve parameters and cost function for both forward and backwards curves
#'
#' @param fitted    Fitted data.frame as given by \code{\link{fitwafer}}.
#' 
#' @return None
#'
#' @examples
#' fitted = fitwafer(wafer3737)
#' histplot(fitted)
#'
#'@importFrom graphics grid hist legend lines mtext par plot
#'
#' @export
histplot = function(fitted){
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  
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

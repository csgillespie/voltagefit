## function to plot histograms of the parameters and cost function 
## for both forward and backwards curves
##
## fitall is of the form fitall

histplot = function(fitall){
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  
  par(mfrow = c(2, 3))
  for (i in 1:6){
    hist(fitall[fitall$direction=="Forward",][,i+5], panel.first = grid(), xlab = bquote(theta[.(i)]), main = "")
  }
  par(mfrow = c(1, 1))
  mtext("Forward", NORTH <- 3, line = 0.7, cex = 1)
  readline("Press enter for the next plot")  
  
  hist(fitall[fitall$direction=="Forward",]$cost, panel.first = grid(), xlab = "cost", main = "Forward")
  readline("Press enter for the next plot") 
  
  par(mfrow = c(2, 3))
  for (i in 1:6){
    hist(fitall[fitall$direction=="Backward",][,i+5], panel.first = grid(), xlab = bquote(theta[.(i)]), main = "")
  }
  par(mfrow = c(1, 1))
  mtext("Backward", NORTH <- 3, line = 0.7, cex = 1)
  readline("Press enter for the next plot") 
  
  hist(fitall[fitall$direction=="Backward",]$cost, panel.first = grid(),xlab = "cost",main = "Backward")
}

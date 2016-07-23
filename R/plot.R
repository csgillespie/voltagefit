## function to plot curves for each week
##
## fitall is of the form, fitall
## curves is of the form, weekcurve, 
## fm is of the form, fitmanova
## orig is whether the plot should be on the original data scale or 
## the transformed scale, default is the original scale, 
## v is the voltage gate measurements (for one device) to be plotted against

plotweek = function(fitall, curves, fm, orig=TRUE, v){
  tot_f = which(diff(v) < 0)[1]
  
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  
  if (orig){
    m = max(fitall$max)
    curves$forward = exp(m / curves$forward)
    curves$backward = exp(m / curves$backward)
  }
  limits = range(curves$backward, curves$forward)
  
  plot(v_f, curves$forward[1,], type = "l", xlab = "Vg (V)", ylab = "Id (A)", log = "y", 
       ylim = limits, main = ifelse(orig, "", "Transformed scale"), panel.first = grid(), col = "white")
  
  for (i in 1:nrow(curves$forward)){
    lines(v_f, curves$forward[i,], col = i)
    lines(v_b, curves$backward[i,], col = i, lty = 2)
  }
  
  temp = paste("week", unique(fm$weight$forward$xlevels$`factor(week)`))
  legend("topleft", temp, col = 1:length(unique(fm$weight$forward$xlevels$`factor(week)`)), lty = 1)
  legend("bottomright", c("Forwards", "Backwards"), col = 1, lty = 1:2)
}

## function to create underlying forward curve
##
## fitall is of the form, fitall
## curves is of the form, undercurve, 
## orig is whether the plot should be on the original data scale or 
## the transformed scale, default is the original scale, 
## med is whether the mean or the median should be used, the default 
## is the mean curve,
## v is the voltage gate measurements (for one device) to be plotted against

## N.B. using the mean with low levels of data can lead to splurious curves, 
## the median appears to be more stable!

plotunder = function(fitall, curves, orig=T, med=F, v){
  tot_f = which(diff(v) < 0)[1]
  
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  op = par(mar=c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  
  if (orig){
    m = max(fitall$max)
    curves$forward = exp(m / curves$forward)
    curves$backward = exp(m / curves$backward)
  }
  limits = range(curves$backward, curves$forward)
  
  if(med){
    means_f = colQuantiles(curves$forward, probs = 0.5)
    means_b = colQuantiles(curves$backward, probs = 0.5)
    temp = c("Median", "95% Interval")
  }
  else{
    means_f = colMeans(curves$forward)
    means_b = colMeans(curves$backward)
    temp = c("Mean", "95% Interval")
  }
  
  lower_f = colQuantiles(curves$forward, probs=0.025)
  upper_f = colQuantiles(curves$forward, probs=0.975)
  lower_b = colQuantiles(curves$backward, probs=0.025)
  upper_b = colQuantiles(curves$backward, probs=0.975)
  
  plot(v_f, means_f, type = "l", xlab = "Vg (V)", ylab = "Id (A)", log = "y", 
       ylim = limits, main = ifelse(orig, "", "Transformed scale"), panel.first = grid())
  lines(v_f, lower_f, lty = 2)
  lines(v_f, upper_f, lty = 2)
  lines(v_b, means_b, col = 2)
  lines(v_b, lower_b, lty = 2, col = 2)
  lines(v_b, upper_b, lty = 2, col = 2)
  
  legend("topleft", temp, col = 1, lty = 1:2)
  legend("bottomright", c("Forwards", "Backwards"), col = 1:2, lty = 1)
}

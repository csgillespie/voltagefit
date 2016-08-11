#' Plot curves for each week
#'
#' Plot curves for each week
#'
#' @inheritParams   fitmanova
#' @inheritParams   plotunder
#' @param wcurves   Week curves as given by \code{\link{curves}}.
#' @param fm        Fitted manova as given by \code{\link{fitmanova}}.
#' 
#' @return None. This function produces a plot.
#' @examples
#' wafers_folder = paste(path.package("voltagefit"),"/extdata/",sep="") # path to wafters data directory
#' fitted = fitall(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = c(1:6), treatment = rep(1:6))
#' fitman = fitmanova(fitted, design)
#' weekp = weekparam(fitman)
#' wcurves = curves(fitted, weekp)
#' plotweek = function(fitted, wcurves, fitman)
#'
#' @export
plotweek = function(fitted, wcurves, fm, orig=TRUE){
  v = attr(fitted,"v")
  tot_f = which(diff(v) < 0)[1]
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  op = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  
  if (orig){
    m = max(fitted$max) ## transformation to put back on original scale
    wcurves$forward = exp(m / wcurves$forward)
    wcurves$backward = exp(m / wcurves$backward)
  }
  limits = range(wcurves$backward, wcurves$forward)
  
  plot(v_f, wcurves$forward[1,], type = "l", xlab = "Vg (V)", ylab = "Id (A)", log = "y", 
       ylim = limits, main = ifelse(orig, "", "Transformed scale"), panel.first = grid(), col = "white")
  
  for (i in 1:nrow(wcurves$forward)){
    lines(v_f, wcurves$forward[i,], col = i)
    lines(v_b, wcurves$backward[i,], col = i, lty = 2)
  }
  
  temp = paste("week", unique(fm$forward$man_w$xlevels$`factor(week)`))
  legend("topleft", temp, col = 1:length(unique(fm$forward$man_w$xlevels$`factor(week)`)), lty = 1)
  legend("bottomright", c("Forwards", "Backwards"), col = 1, lty = 1:2)
}

#' Plot underlying curve
#'
#' Plot underlying curve
#' N.B. using the mean with low levels of data can lead to spurious curves, 
#' the median appears to be more stable!
#' 
#' @inheritParams   fitmanova
#' @param underc    Underlying curve as given by \code{\link{curves}}.
#' @param med       Boolean value deciding whether the whether the median should be used rather than mean (Default: FALSE)
#' @param orig      Boolean value deciding whether the plot should be on the original data scale or the transformed scale (Default: TRUE)
#' 
#' @return None. This function produces a plot.
#' @examples
#' wafers_folder = paste(path.package("voltagefit"),"/extdata/",sep="") # path to wafers data directory
#' fitted = fitall(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = c(1:6), treatment = rep(1:6))
#' fitman = fitmanova(fitted, design)
#' underp = underparam(fitman)
#' unders = undercurvesim(underp)
#' underc = curves(fitted, unders)
#' plotunder(fitted, underc)
#' 
#' @importFrom matrixStats colQuantiles
#'
#' @export
plotunder = function(fitted, underc, orig=T, med=F){
  v = attr(fitted,"v")
  tot_f = which(diff(v) < 0)[1]
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  op = par(mar=c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01, cex.axis = 0.9, las = 1)
  on.exit(op)
  
  if (orig){
    m = max(fitted$max) ## transformation to put back on original scale
    underc$forward = exp(m / underc$forward)
    underc$backward = exp(m / underc$backward)
  }
  limits = range(underc$backward, underc$forward)
  
  if(med){
    ## calculate median curve if required
    means_f = colQuantiles(underc$forward, probs = 0.5)
    means_b = colQuantiles(underc$backward, probs = 0.5)
    temp = c("Median", "95% Interval")
  }
  else{
    ## calculate mean curve if required
    means_f = colMeans(underc$forward)
    means_b = colMeans(underc$backward)
    temp = c("Mean", "95% Interval")
  }
  
  ## calculate 95% region of underlying curves
  lower_f = colQuantiles(underc$forward, probs=0.025)
  upper_f = colQuantiles(underc$forward, probs=0.975)
  lower_b = colQuantiles(underc$backward, probs=0.025)
  upper_b = colQuantiles(underc$backward, probs=0.975)
  
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

#' Function to create curves
#'
#' Function to create curves
#'
#' @inheritParams fitmanova
#' @param param   Samples as given by \code{\link{undercurvesim}} or \code{\link{weekparam}}.
#' 
#' @return A list containing elements forward and backward, each containing the curves for each week (one curve per row).
#' 
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fitted = fitall(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = 1:6, treatment = rep(1,6))
#' fitman = fitmanova(fitted, design)
#' underp = underparam(fitman)
#' param = undercurvesim(underp)
#' curves(fitted,param)
#'
#' @export
curves = function(fitted, param){
  v = attr(fitted,"v")
  tot_f = which(diff(v) < 0)[1]
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  ## get week parameters for forward and backward
  param_f = param[param$direction=="Forward",grep("X[0-9]+",colnames(param))]
  param_b = param[param$direction=="Backward",grep("X[0-9]+",colnames(param))]
  
  curves_f = matrix(0, nrow=nrow(param_f), ncol=length(v_f))
  curves_b = matrix(0, nrow=nrow(curves_f), ncol=length(v_b))
  
  for (i in 1:nrow(curves_f)){
    ## calculate curves corresponding to parameters
    curves_f[i,] = logcurve(v_f, as.numeric(param_f[i,]))
    curves_b[i,] = logcurveb(v_b, as.numeric(param_f[i,]), as.numeric(param_b[i,]))
  }
  
  return(list(forward = as.data.frame(curves_f), backward = as.data.frame(curves_b)))
}

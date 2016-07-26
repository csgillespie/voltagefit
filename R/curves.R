## function to create curves 
##
## fitall is of the form fitall,
## param is of the form, weekparam or undercurvesim
##
## the function returns a list containing elements forward and 
## backward, each containing the curves for each week (one curve per row)

curves = function(fitall, param){
  v = attr(fitall,"v")
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

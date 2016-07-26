## function to create curves for each week
##
## fitall is of the form fitall,
## weekp is of the form, weekparam
##
## the function returns a list containing elements forward and 
## backward, each containing the curves for each week (one curve per row)

weekcurve = function(fitall, weekp){
  v = attr(fitall,"v")  ## get v_gate as attribute from fitall
  tot_f = which(diff(v) < 0)[1] ## calculate how many readings make up the forward pass
  v_f = v[1:tot_f] ## get v for forward pass
  v_b = v[tot_f:length(v)] ## get v for backward pass
  
  ## get week parameters for forward and backward
  param_f = weekp[weekp$direction=="Forward",3:8] 
  param_b = weekp[weekp$direction=="Backward",3:8]
  
  curves_f = matrix(0, nrow=nrow(param_f), ncol=length(v_f))
  curves_b = matrix(0, nrow=nrow(curves_f), ncol=length(v_b))
  
  for (i in 1:nrow(curves_f)){
    ## calculate curves corresponding to each week
    curves_f[i,] = logcurve(v_f, as.numeric(param_f[i,]))
    curves_b[i,] = logcurveb(v_b, as.numeric(param_f[i,]), as.numeric(param_b[i,]))
  }
  
  return(list(forward = curves_f, backward = curves_b))
}


## function to create underlying curve
##
## fitall is of the form fitall,
## underp is of the form, undercurvesim, 
##
## the function returns a list containing elements forward and 
## backward, each containing the sample of underlying curves (one curve per row)

undercurve = function(fitall ,underp){
  v = attr(fitall,"v")  ## get v_gate as attribute from fitall
  tot_f = which(diff(v) < 0)[1] ## calculate how many readings make up the forward pass
  v_f = v[1:tot_f] ## get v for forward pass
  v_b = v[tot_f:length(v)] ## get v for backward pass
  
  ## get underlying parameters for forward and backward
  param_f = underp[underp$direction=="Forward",2:7]
  param_b = underp[underp$direction=="Backward",2:7]
  
  curves_f = matrix(0, nrow=nrow(param_f), ncol=length(v_f))
  curves_b = matrix(0, nrow=nrow(curves_f), ncol=length(v_b))
  
  for (i in 1:nrow(curves_f)){
    ## calculate underlying curve for each generated set of parameters 
    curves_f[i,] = logcurve(v_f, as.numeric(param_f[i,]))
    curves_b[i,] = logcurveb(v_b, as.numeric(param_f[i,]), as.numeric(param_b[i,]))
  }
  return(list(forward = curves_f, backward = curves_b))
}

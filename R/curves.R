## function to create curves for each week
##
## weekp is of the form, weekparam
## v is the voltage gate measurements (for one device)

weekcurve = function(weekp, v){
  tot_f = which(diff(v) < 0)[1]
  
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  param_f = weekp[weekp$direction=="Forward",3:8]
  param_b = weekp[weekp$direction=="Backward",3:8]
  
  curves_f = matrix(0, nrow=nrow(param_f), ncol=length(v_f))
  curves_b = matrix(0, nrow=nrow(curves_f), ncol=length(v_b))
  
  for (i in 1:nrow(curves_f)){
    curves_f[i,] = logcurve(v_f, as.numeric(param_f[i,]))
    curves_b[i,] = logcurveb(v_b, as.numeric(param_f[i,]), as.numeric(param_b[i,]))
  }
  
  return(list(forward = curves_f, backward = curves_b))
}


## function to create underlying forward curve
##
## underp is of the form, undercurvesim, 
## v is the voltage gate measurements (for one device)

undercurve = function(underp, v){
  tot_f = which(diff(v) < 0)[1]
  
  v_f = v[1:tot_f]
  v_b = v[tot_f:length(v)]
  
  param_f = underp[underp$direction=="Forward",2:7]
  param_b = underp[underp$direction=="Backward",2:7]
  
  curves_f = matrix(0, nrow=nrow(param_f), ncol=length(v_f))
  curves_b = matrix(0, nrow=nrow(curves_f), ncol=length(v_b))
  
  for (i in 1:nrow(curves_f)){
    curves_f[i,] = logcurve(v_f, as.numeric(param_f[i,]))
    curves_b[i,] = logcurveb(v_b, as.numeric(param_f[i,]), as.numeric(param_b[i,]))
  }
  return(list(forward = curves_f, backward = curves_b))
}

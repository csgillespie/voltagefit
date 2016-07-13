## function to create curves for each week
##
## weekp is of the form, weekparam
## v is the voltage gate measurements (for one device)

weekcurve = function(weekp,v)
{
  tot_f = which(diff(v)<0)[1]
  
  v_f=v[1:tot_f]
  v_b=v[tot_f:length(v)]
  
  curves_f=matrix(0,nrow=nrow(weekp$forward),ncol=length(v_f))
  for(i in 1:nrow(curves_f))
  {
    curves_f[i,]=logcurve(v_f,weekp$forward[i,])
  }
  
  curves_b=matrix(0,nrow=nrow(weekp$forward),ncol=length(v_b))
  for(i in 1:nrow(curves_b))
  {
    curves_b[i,]=logcurveb(v_b,weekp$forward[i,],weekp$backward[i,])
  }
  
  return(list(forward=curves_f,backward=curves_b))
}


## function to create underlying forward curve
##
## underp is of the form, undercurvesim, tot_f is the total 
## number of observations made in the forward pass on each device 
## within a wafer (default=193)

undercurve = function(wafer,underp,tot_f=193)
{
  tot_obs = length(subset(subset(wafer, wafer_id %in% unique(wafer$wafer_id)[1]),
                                   name %in% unique(wafer$name)[1])$VG)

  v_f=wafer$VG[1:tot_f]
  v_b=wafer$VG[tot_f:tot_obs]

  curves_f=matrix(0,nrow=nrow(underp$forward),ncol=length(v_f))
  curves_b=matrix(0,nrow=nrow(underp$forward),ncol=length(v_b))
  
  for(i in 1:nrow(underp$forward))
  {
    curves_f[i,]=logcurve(v_f,underp$forward[i,])
  }
  
  for(i in 1:nrow(underp$forward))
  {
    curves_b[i,]=logcurveb(v_b,underp$forward[i,],underp$backward[i,])
  }
    
  return(list(forward=curves_f,backward=curves_b))
}

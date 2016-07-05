## function to create curves for each week
##
## fited is of the form, fitwafer, weekp is of the form, weekparam, tot_obs is the 
## total number of observations made on each device within a wafer 
## (default=241), tot_f is the total number of observations made in 
## the forward pass on each device within a wafer (default=193)

weekcurve = function(wafer,fitted,weekp,tot_obs=241,tot_f=193)
{
  v_f=wafer$VG[1:tot_f]
  v_b=wafer$VG[tot_f:tot_obs]
  
  curves_f=matrix(0,nrow=length(unique(wafer$test_date)),ncol=length(v_f))
  for(i in 1:nrow(curves_f))
  {
    curves_f[i,]=logcurve(v_f,weekp$forward[i,])
  }
  
  curves_b=matrix(0,nrow=length(unique(wafer$test_date)),ncol=length(v_b))
  for(i in 1:nrow(curves_b))
  {
    curves_b[i,]=logcurveb(v_b,weekp$forward[i,],weekp$backward[i,])
  }
  
  return(list(forward=curves_f,backward=curves_b))
}

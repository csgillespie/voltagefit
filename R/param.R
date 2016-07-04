## function to create a parameter matrix, adding week, wafer, device 
## to the respective parameter values, tot_obs is the total number of 
## observations made on each device within a wafer (default=241)
##
## fited is of the form, fitwafer

thetasetup = function(wafer,fited,tot_obs=241)
{
  w_index = 1:length(unique(wafer$test_date))
  w = NULL
  for(i in 1:length(unique(wafer$test_date)))
  {
    w=c(w,rep(w_index[i],length(which(wafer$test_date==unique(wafer$test_date)[i]))/tot_obs))
  }
  
  w_id=NULL
  for(i in 1:length(unique(wafer$wafer_id)))
  {
    w_id=c(w_id,rep(unique(wafer$wafer_id)[i],length(which(wafer$wafer_id==unique(wafer$wafer_id)[i]))/tot_obs))
  }
  
  n=NULL
  for(i in 1:length(unique(wafer$wafer_id)))
  {
    n=c(n,unique(subset(wafer,wafer_id %in% unique(wafer$wafer_id)[i])$name))
  }
  
  return(list(forward=data.frame(w,w_id,n,fited$forward$parameters),backward=data.frame(w,w_id,n,fited$backward$parameters)))
}

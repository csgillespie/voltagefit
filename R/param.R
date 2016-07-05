## function to create a parameter matrix, adding week, wafer, device 
## to the respective parameter values, tot_obs is the total number of 
## observations made on each device within a wafer (default=241)
##
## fited is of the form, fitwafer

thetasetup = function(wafer,fited,tot_obs=241)
{
  l = numeric(length(unique(wafer$test_date)))
  for(i in 1:length(unique(wafer$test_date)))
  {
    l[i] = length(which(wafer$test_date==unique(wafer$test_date)[i]))/tot_obs
  }
  w = rep(1:length(unique(wafer$test_date)),l)

  l = numeric(length(unique(wafer$wafer_id)))
  for(i in 1:length(unique(wafer$wafer_id)))
  {
    l[i] = length(which(wafer$wafer_id==unique(wafer$wafer_id)[i]))/tot_obs
  }
  w_id = rep(unique(wafer$wafer_id)[1:length(unique(wafer$wafer_id))],l)

  n = numeric(dim(fited$forward$parameters)[1])
  pos = 0 
  for(i in 1:length(unique(wafer$wafer_id)))
  {
    temp = unique(subset(wafer,wafer_id %in% unique(wafer$wafer_id)[i])$name)
    n[(pos+1):(pos+length(temp))] = temp
    pos = pos +length(temp)
  }

  return(list(forward=data.frame(w,w_id,n,fited$forward$parameters),backward=data.frame(w,w_id,n,fited$backward$parameters)))
} 

## function to fit logcurve to the wafer, getting parameters and cost
## wafer is the data to fit to, lim is the max number of iterations to 
## use in nlm (default=2000), 
## tot_f is the total number of observations made in the forward pass 
## on each device within a wafer (default=193)
##
## the function returns a list with elements parameters and cost

fitwafer = function(wafer,lim=2000,tot_f=193)
{
  tot_obs = length(subset(subset(wafer, wafer_id %in% unique(wafer$wafer_id)[1]),
                          name %in% unique(wafer$name)[1])$VG)
  
  estf=matrix(0,nrow=dim(wafer)[1]/tot_obs,ncol=6)
  estb=matrix(0,nrow=dim(wafer)[1]/tot_obs,ncol=6)
  
  minf=numeric(dim(wafer)[1]/tot_obs)
  minb=numeric(dim(wafer)[1]/tot_obs)
  
  place = 0
  
  for(j in unique(wafer$test_date))
  {
    dd_temp = subset(wafer, test_date %in% c(j))
    message("test_date: ",j,"\n")
    for(k in unique(dd_temp$wafer_id))
    {  
      dd = subset(dd_temp, wafer_id %in% c(k))
      message("wafer: ",k,"\n")
      for(i in unique(dd$name))
      {
        place = place+1
        d = subset(dd, name %in% c(i))
        
        d_forward = d[1:tot_f,]
        d_backward = d[tot_f:tot_obs,] 
        
        #forwards
        m.nlmf = nlm(minlogcurve,c(1.0,1.4,-3,4,-1.5,0.3),d_forward$VG,
                     max(log(abs(d_forward$ID)))/log(abs(d_forward$ID)),iterlim=lim) 
        
        estf[place,] = m.nlmf$estimate
        if(m.nlmf$iterations==lim) message(paste("Max reached for forward, device ",j,".",k,".",i))
        minf[place] = minlogcurve(m.nlmf$estimate,d_forward$VG,
                                  max(log(abs(d_forward$ID)))/log(abs(d_forward$ID)))
        
        #backwards
        m.nlmb = nlm(minlogcurveb,c(0.01,0.01,0.01,0.01,0.01,0.01),d_backward$VG,
                     max(log(abs(d_backward$ID)))/log(abs(d_backward$ID)),m.nlmf$estimate,iterlim=lim)
        
        estb[place,] = m.nlmb$estimate
        if(m.nlmb$iterations==lim) message(paste("Max reached for backward, device ",j,".",k,".",i))
        minb[place] = minlogcurveb(m.nlmb$estimate,d_backward$VG,
                                   max(log(abs(d_backward$ID)))/log(abs(d_backward$ID)),m.nlmf$estimate)
      }
    }
  }
  return(list(forward=list(parameters=estf,cost=minf),backward=list(parameters=estb,cost=minb)))
}

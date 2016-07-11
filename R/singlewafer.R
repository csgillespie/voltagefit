## function to fit logcurve to the wafer, getting parameters and cost
## wafer is the data to fit to (a single wafer), lim is the max number 
## of iterations to use in nlm (default=2000), 
## tot_f is the total number of observations made in the forward pass 
## on each device within a wafer (default=193)
##
## the function returns a list with elements parameters and cost

fitwafer = function(wafer,lim=2000,tot_f=193)
{
  tot_obs = length(subset(wafer,name %in% unique(wafer$name)[1])$VG)
  
  estf = matrix(0,nrow=dim(wafer)[1]/tot_obs,ncol=6)
  estb = matrix(0,nrow=dim(wafer)[1]/tot_obs,ncol=6)
  
  minf = numeric(dim(wafer)[1]/tot_obs)
  minb = numeric(dim(wafer)[1]/tot_obs)
  
  place = 0
  
  for(i in unique(wafer$name))
  {
    place = place+1
    d = subset(wafer, name %in% c(i))
    
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
  return(list(forward=list(parameters=estf,cost=minf),backward=list(parameters=estb,cost=minb)))
}


## function to perform fitwafer on multiple wafers, getting parameters and cost
## path is the directory where multiple files (containing multiple wafers)
## are located, lim is the max number of iterations to use in nlm (default=2000), 
## tot_f is the total number of observations made in the forward pass 
## on each device within a wafer (default=193)
##
## the function returns a list with elements parameters and cost

fitall = function(path,lim=2000,tot_f=193)
{
  files = list.files(path = path, pattern = ".rds")
  message("files read: ",paste(files,collapse=" "))

  estf = NULL
  estb = NULL  
  minf = NULL
  minb = NULL
  
  for(i in 1:length(files))
  {
    dat1 <- readRDS(files[i]) 
    df <- cSplit(dat1, c("VG", "ID"), sep = ",", "long") #think about this line as may not come like this

    for(j in unique(df$wafer_id))
    {
      wafer = subset(df, wafer_id %in% c(j))
      message("wafer: ",j)
      temp = fitwafer(wafer,lim,tot_f)
      
      temp1 = cbind(w=rep(i,length(temp$forward$cost)),
                    w_id=rep(j,length(temp$forward$cost)),
                    n=unique(wafer$name))
      
      temp$forward$parameters = data.frame(temp1,temp$forward$parameters)
      temp$backward$parameters = data.frame(temp1,temp$backward$parameters)
      
      estf = rbind(estf,temp$forward$parameters)
      estb = rbind(estb,temp$backward$parameters)
      minf = c(minf,temp$forward$cost)
      minb = c(minb,temp$backward$cost)
    }
  }
  return(list(forward=list(parameters=estf,cost=minf),backward=list(parameters=estb,cost=minb)))
}

## function to fit logcurve to the wafer, getting parameters and cost
## wafer is the data to fit to (a single wafer), lim is the max number 
## of iterations to use in nlm (default=2000), id is the wafer id (file name)
##
## the function returns a list with elements parameters and cost

fitwafer = function(wafer,lim=2000,id=NULL)
{
  if(is.null(id)){id = randomStrings(n=1, len=5)} #this feels slow, poss other function?
  else{id=gsub(".rds","",id)}
  
  tot_obs = length(subset(wafer,name %in% unique(wafer$name)[1])$VG)
  tot_f = which(diff(subset(wafer,name %in% unique(wafer$name)[1])$VG)<0)[1]

  estf = matrix(0,nrow=dim(wafer)[1]/tot_obs,ncol=6)
  estb = estf
  
  minf = matrix(0,nrow=dim(wafer)[1]/tot_obs,ncol=1)
  minb = minf
  
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
    minf[place,] = minlogcurve(m.nlmf$estimate,d_forward$VG,
                              max(log(abs(d_forward$ID)))/log(abs(d_forward$ID)))
    
    #backwards
    m.nlmb = nlm(minlogcurveb,c(0.01,0.01,0.01,0.01,0.01,0.01),d_backward$VG,
                 max(log(abs(d_backward$ID)))/log(abs(d_backward$ID)),m.nlmf$estimate,iterlim=lim)
    
    estb[place,] = m.nlmb$estimate
    if(m.nlmb$iterations==lim) message(paste("Max reached for backward, device ",j,".",k,".",i))
    minb[place,] = minlogcurveb(m.nlmb$estimate,d_backward$VG,
                               max(log(abs(d_backward$ID)))/log(abs(d_backward$ID)),m.nlmf$estimate)
  }
  return(list(forward=list(parameters=data.frame(id=rep(id,dim(estf)[1]),name=unique(wafer$name),max=rep(max(log(abs(wafer$ID))),dim(estf)[1]),estf)
                           ,cost=data.frame(id=rep(id,dim(estf)[1]),name=unique(wafer$name),cost=minf)),
              backward=list(parameters=data.frame(id=rep(id,dim(estb)[1]),name=unique(wafer$name),max=rep(max(log(abs(wafer$ID))),dim(estf)[1]),estb)
                            ,cost=data.frame(id=rep(id,dim(estb)[1]),name=unique(wafer$name),cost=minb))))
}

## function to perform fitwafer on multiple wafers, getting parameters and cost
## path is the directory where multiple files (each containing a single wafer)
## are located, lim is the max number of iterations to use in nlm (default=2000), 
##
## the function returns a list with elements parameters and cost
## each wafer should be suitably filtered and formated before been fed in

fitall = function(path,lim=2000)
{
  files = list.files(path = path, pattern = ".rds")
  message("files read: ",paste(files,collapse=" "))

  for(i in 1:length(files))
  {
    wafer = readRDS(paste(path,files[i],sep="")) 
    message("wafer: ",files[i])
    temp = fitwafer(wafer,lim,files[i])
    
    if(i==1)
    {
      estf = temp$forward$parameters
      estb = temp$backward$parameters
      minf = temp$forward$cost
      minb = temp$backward$cost
    }
    else
    {
      estf = rbind(estf,temp$forward$parameters)
      estb = rbind(estb,temp$backward$parameters)
      minf = rbind(minf,temp$forward$cost)
      minb = rbind(minb,temp$backward$cost)
    }
  }
  return(list(forward=list(parameters=estf,cost=minf),backward=list(parameters=estb,cost=minb)))
}

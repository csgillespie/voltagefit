## function to fit logcurve to the wafer, getting parameters and cost
## wafer is the data to fit to (a single wafer), iterlim is the max number 
## of iterations to use in nlm (default=2000), id is the wafer id (file name)
##
## the function returns a list with elements parameters and cost

fitwafer = function(wafer, iterlim=2000, id=NULL){
  if (is.null(id)){
    id = stri_rand_strings(1, 5)
  } 
  else{
    id = gsub(".rds","",id)
  }
  
  tot_obs = length(subset(wafer, name %in% unique(wafer$name)[1])$VG)
  tot_f = which(diff(subset(wafer, name %in% unique(wafer$name)[1])$VG) < 0)[1]
  
  estf = matrix(0, nrow = dim(wafer)[1] / tot_obs, ncol = 7)
  estb = estf

  place = 0  
  for (i in unique(wafer$name))
  {
    place = place + 1
    d = subset(wafer, name %in% c(i))
    
    d_forward = d[1:tot_f,]
    d_backward = d[tot_f:tot_obs,] 
    
    #forwards
    m.nlmf = nlm(minlogcurve, c(1.0, 1.4, -3, 4, -1.5, 0.3), d_forward$VG,
                 max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)), iterlim = iterlim) 
    
    estf[place,1:6] = m.nlmf$estimate
    if(m.nlmf$iterations==iterlim) message(paste("Max reached for forward, device ",j,".",k,".",i))
    estf[place,7] = minlogcurve(m.nlmf$estimate, d_forward$VG,
                               max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)))
    
    #backwards
    m.nlmb = nlm(minlogcurveb, c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01), d_backward$VG,
                 max(log(abs(d_backward$ID))) / log(abs(d_backward$ID)), m.nlmf$estimate, iterlim = iterlim)
    
    estb[place,1:6] = m.nlmb$estimate
    if(m.nlmb$iterations==iterlim) message(paste("Max reached for backward, device ",j,".",k,".",i))
    estb[place,7] = minlogcurveb(m.nlmb$estimate, d_backward$VG,
                                max(log(abs(d_backward$ID))) / log(abs(d_backward$ID)), m.nlmf$estimate)
  }
  forward = data.frame(id = id, name = unique(wafer$name), max = max(log(abs(wafer$ID))), 
                        cost = estf[,7], direction = "Forward", estf[,1:6])
  backward = data.frame(id = id, name = unique(wafer$name), max = max(log(abs(wafer$ID))), 
                        cost = estb[,7], direction = "Backward", estb[,1:6])
  
  return(rbind(forward,backward))
}

## function to perform fitwafer on multiple wafers, getting parameters and cost
## path is the directory where multiple files (each containing a single wafer)
## are located, lim is the max number of iterations to use in nlm (default=2000), 
##
## the function returns a list with elements parameters and cost
## each wafer should be suitably filtered and formated before been fed in

fitall = function(path, iterlim=2000, cost){
  files = list.files(path = path, pattern = ".rds")
  message("files read: ",paste(files,collapse=" "))
  
  for(i in 1:length(files)){
    wafer = readRDS(paste(path, files[i], sep="")) 
    message("wafer: ",files[i])
    fit = fitwafer(wafer, iterlim, files[i])
    
    if(i==1){
      param = fit
    }
    else{
      param = rbind(param, fit)
    }
  }
  return(param)
}

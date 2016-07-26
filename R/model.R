## function to fit logcurve to the wafer, getting parameters and cost
## for the forward and backwards curve
## wafer is the data to fit to (a single wafer), iterlim is the max number 
## of iterations to use in nlm (default=2000), id is the wafer id (file name)
## if no id is specified a random string is generated as the id
##
## the function returns a data.frame looking like 
##    id       name       max       cost direction        X1        X2        X3       X4        X5        X6
## 1 3737 19.13.03.B -8.778558 0.05164936   Forward 0.9498490 0.3560538 -3.442883 4.289813 -1.857420 0.3103374
## 2 3737 25.16.03.B -8.778558 0.05494473   Forward 0.9501822 0.3588713 -3.458188 4.341372 -1.878082 0.3123540
## 3 3737 25.13.03.B -8.778558 0.05214698   Forward 0.9492697 0.3559791 -3.276120 4.130474 -1.788644 0.3001555
##
## id = wafer id
## name = each device on the wafer
## max = maximum of log(abs(wafer$ID)), ID = current drain
## cost = the value of costfun for each device
## direction = whether it is the forward or backward curve
## X1 - X6 = the parameters characterising the curves

fitwafer = function(wafer, iterlim=2000, id=NULL){
  if (is.null(id)){
    id = stri_rand_strings(1, 5)  ## check wafer has an id, if not generate one
  } 
  else{
    id = gsub(".rds","",id)
  }
  
  ## get total observations on a device and total number in the forward pass
  tot_obs = length(wafer[wafer$name==unique(wafer$name)[1],]$VG) 
  tot_f = which(diff(wafer[wafer$name==unique(wafer$name)[1],]$VG) < 0)[1]
  
  estf = matrix(0, nrow = dim(wafer)[1] / tot_obs, ncol = 7)
  estb = estf

  place = 0  
  for (i in unique(wafer$name)) ## iterate through each device on the wafer
  {
    place = place + 1
    d = wafer[wafer$name==i,]  ## get the data for the device
    
    d_forward = d[1:tot_f,]  ## get the forward pass
    d_backward = d[tot_f:tot_obs,]  ## get the backward pass
    
    #forwards
    ## fit the model to the forward pass, fitted to transformed data to improve fit
    m.nlmf = nlm(minlogcurve, c(1.0, 1.4, -3, 4, -1.5, 0.3), d_forward$VG,
                 max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)), iterlim = iterlim) 
    estf[place,1:6] = m.nlmf$estimate  ## store parameters 
    ## warning if the model doesn't fit within the specified iterations 
    if(m.nlmf$iterations==iterlim) message(paste("Max reached for forward, device ",j,".",k,".",i))
    ## calculate the final cost and store
    estf[place,7] = minlogcurve(m.nlmf$estimate, d_forward$VG,
                               max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)))
    
    #backwards (as above but for backwards pass)
    m.nlmb = nlm(minlogcurveb, c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01), d_backward$VG,
                 max(log(abs(d_backward$ID))) / log(abs(d_backward$ID)), m.nlmf$estimate, iterlim = iterlim)
    estb[place,1:6] = m.nlmb$estimate
    if(m.nlmb$iterations==iterlim) message(paste("Max reached for backward, device ",j,".",k,".",i))
    estb[place,7] = minlogcurveb(m.nlmb$estimate, d_backward$VG,
                                max(log(abs(d_backward$ID))) / log(abs(d_backward$ID)), m.nlmf$estimate)
  }
  ## combine into data.frame
  forward = data.frame(id = id, name = unique(wafer$name), max = max(log(abs(wafer$ID))), 
                        cost = estf[,7], direction = "Forward", estf[,1:6])
  backward = data.frame(id = id, name = unique(wafer$name), max = max(log(abs(wafer$ID))), 
                        cost = estb[,7], direction = "Backward", estb[,1:6])
  
  return(rbind(forward,backward))
}


## function to perform fitwafer on multiple wafers, getting parameters and cost
## for the forward and backwards curve
## path is the directory where multiple files (each containing a single wafer)
## are located, lim is the max number of iterations to use in nlm (default=2000), 
##
## the function returns a data.frame looking like 
##    id       name       max       cost direction        X1        X2        X3       X4        X5        X6
## 1 3737 19.13.03.B -8.778558 0.05164936   Forward 0.9498490 0.3560538 -3.442883 4.289813 -1.857420 0.3103374
## 2 3737 25.16.03.B -8.778558 0.05494473   Forward 0.9501822 0.3588713 -3.458188 4.341372 -1.878082 0.3123540
## 3 3737 25.13.03.B -8.778558 0.05214698   Forward 0.9492697 0.3559791 -3.276120 4.130474 -1.788644 0.3001555
##
## id = wafer id
## name = each device on the wafer
## max = maximum of log(abs(wafer$ID)), ID = current drain
## cost = the value of costfun for each device
## direction = whether it is the forward or backward curve
## X1 - X6 = the parameters characterising the curves
##
## we append the attribute v (voltage gate readings for one device)
## used in the curve functions and plotting functions 
##
## That is an extended version of fitwafer
##
## N.B. It is assumed that any wafer entering this function (and 
## subsequently fitwafer) has been pre-filtered 
##
## The files contained within the directory have been pre-filtered. 
## See the function "filtered" for details of how the wafer was filtered 

fitall = function(path, iterlim=2000, cost){
  files = list.files(path = path, pattern = ".rds") ## get files to read
  message("files read: ",paste(files,collapse=" "))
  
  for(i in 1:length(files)){
    wafer = readRDS(paste(path, files[i], sep="")) ## read a file/wafer
    message("wafer: ",files[i])
    fit = fitwafer(wafer, iterlim, files[i]) ## fit model to that wafer
    
    if(i==1){
      param = fit
      v = wafer[wafer$name==unique(wafer$name)[1],]$VG ## get v_gate reading, used in later functions and plotting
    }
    else{
      param = rbind(param, fit)
    }
  }
  attr(param,"v") = v
  return(param)
}

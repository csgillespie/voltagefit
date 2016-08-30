

#' Determine forward/backward curve
#' 
#' Adds a new column called `direction` indicating if the row relates
#' to forward or backward.
#' @inheritParams fit_wafer
#' @export
add_forward_backward = function(wafer) {
  i = unique(wafer$name)[1]
  dd = NULL
  for (i in unique(wafer$name)){ ## iterate through each device on the wafer
    d = wafer[wafer$name==i,]  ## get the data for the device
    tot_f = which(diff(wafer[wafer$name==i,]$VG) < 0)[1]
    tot_obs = length(wafer[wafer$name==unique(wafer$name)[1],]$VG) 
    d$direction = rep(c("Forward", "Backward"), c(tot_f-1, tot_obs - tot_f + 1))
    dd = rbind(dd, d)
  }
  return(dd)
}

#' @rdname fit_wafer
#' @export
trans_device = function(wafer) {
  wafer$ID = pmax(wafer$ID, 1e-13)
  wafer$ID = pmin(wafer$ID, 1e-3)
  wafer
}

#' @rdname fit_wafer
#' @export
validate_device = function(wafer) {
  wafer = wafer[wafer$direction == "Forward", ]
  ID = wafer[wafer$VG < -5, "ID"]
  if(any(ID > 1e-9)) return(FALSE)
  
  ID = wafer[wafer$VG > 5, "ID"]
  if(any(ID < 1e-5)) return(FALSE)
  return(TRUE)
}


#' Fit logcurves to a wafer
#'
#' Fits logcurves to a wafer and returns the curve parameters and resulting
#' cost for both forward and backwards curves. It is assumed that any wafer
#' entering this function has been pre-filtered. See the function "filtered"
#' for details of how wafers are filtered.
#'
#' @param wafer     Data from a single wafer.
#' @param trans     Wafer ransform function
#' @param validate     Wafer validate function
#' @param initparams  Inital parameter guess for use in optimiser
#' @param maxit   Maxiumum number of iterations for use in optimiser (Default: 10000).
#' @param verbose Print verbose output (Default: \code{TRUE}).
#' 
#' @return A data.frame consisting of the fields:
#'   \describe{
#'      \item{id}{Wafer ID}
#'      \item{name}{Each device on the wafer}
#'      \item{cost}{The value of the cost function for each device}
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{X1 ... X6}{The parameters characterising the curve}
#'   }
#'
#' @examples
#' fit_wafer(wafer3737)
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom stats lm manova vcov
#' @importFrom MASS mvrnorm
#' @importFrom stats coefficients optim weighted.mean
#' @importFrom graphics axis
#' 
#' @export
fit_wafer = function(wafer, trans=trans_device, validate=validate_device,
                     initparams = c(-10,-30,5,0,5), maxit=10000, verbose=TRUE){
  wafer = add_forward_backward(wafer)
  npars = length(initparams);
  estf = matrix(NA, nrow = length(unique(wafer$name)), ncol = 7)
  estb = estf
  i = unique(wafer$name)[1]
  place = 0  
  #initparams = c(-9.1123, -15.5432, 1.1966,0.6834, -0.1843, 0.1418)
  
  for (i in unique(wafer$name)){ ## iterate through each device on the wafer
    if(verbose) message(i)
    place = place + 1
    d = wafer[wafer$name==i,]  ## get the data for the device
    d = trans(d)
    is_valid = validate(d)
    
    if(is_valid) {
      d_forward = d[d$direction == "Forward",]  
      #forwards
      datax = d_forward$VG; datay = log(d_forward$ID)
      est = optim(initparams, min_logcurve, datax=datax, datay=datay, control=list(maxit=maxit))
      estf[place, 1:npars] = cur_forward_pars = est$par
      
      # if(m.nlmf$iterations==iterlim) message(paste("Max reached for forward, device ", i))
      estf[place, ncol(estf)] = est$value
      
      d_backward = d[d$direction == "Backward",]
      datax = d_backward$VG; datay= log(d_backward$ID)
      est = optim(cur_forward_pars, min_logcurve, datax=datax, datay=datay, control=list(maxit=maxit))
      estb[place, 1:npars] = cur_backward_pars = est$par
      #if(m.nlmb$iterations==iterlim) message(paste("Max reached for backward, device ",i))
      estb[place, ncol(estb)] = est$value
    }
  }
  
  for_params = apply(estf[, 1:npars, drop=FALSE], 2, weighted.mean, estf[,7], na.rm=TRUE)
  back_params = apply(estb[, 1:npars, drop=FALSE], 2, weighted.mean, estb[,7], na.rm=TRUE)
  
  results = data.frame(rbind(for_params, back_params), 
                       cost = c(mean(estf[,7]), mean(estb[,7])),
                       id = wafer$wafer_id[1], 
                       direction= c("Forward", "Backward"), stringsAsFactors = FALSE)
  rownames(results) = NULL
  
  
  class(results) = c("wafer", class(results))
  return(results)
}

#' Fit logcurves to multiple wafers
#'
#' Fits logcurves to multiple wafers in a directory, and returns the parameters
#' and cost for both forward and backwards curves. This is an extended version
#' of \code{\link{fit_wafer}}. It is assumed that any wafer entering this
#' function (and subsequently fit_wafer) has been pre-filtered. See the function
#' "filtered" for details of how wafers are filtered.
#'
#' @inheritParams  fit_wafer
#' @param path     Directory containing multiple .rds files, each containing a single wafer
#' 
#' @return A data.frame consisting of the fields:
#'   \describe{
#'      \item{id}{Wafer ID}
#'      \item{name}{Each device on the wafer}
#'      \item{max}{Maximum of \code{log(abs(ID))}, where \code{ID} is the current drain data taken from \code{wafer}}
#'      \item{cost}{The value of the cost function for each device}
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{X1 ... X6}{The parameters characterising the curve}
#'   }
#'   The attribute v (voltage gate readings for one device), used in curve
#'   functions and plotting functions, is also appended.
#'
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fit = fit_all(wafers_folder)
#'
#' @export
fit_all = function(path, maxit=10000, verbose=TRUE){
  files = list.files(path = path, pattern = ".rds") ## get files to read
  if(verbose) message("Files found: ", paste(files,collapse=" "))
  
  for(i in 1:length(files)){
    wafer = readRDS(file.path(path, files[i])) ## read a file/wafer
    if(verbose) message("Reading: ", files[i])
    fit = fit_wafer(wafer, maxit=maxit, verbose=verbose) ## fit model to that wafer
    
    if(i==1){
      param = fit
      v = wafer[wafer$name==unique(wafer$name)[1],]$VG ## get v_gate reading, used in later functions and plotting
    } else {
      param = rbind(param, fit)
    }
  }
  attr(param,"v") = v
  return(param)
}

#' fit_wafer_and_plot
#' Fits given logcurves a wafer and plots the devices along with each fit.
#' @inheritParams fit_wafer
#' @param logc     logcurve function
#' @param minlogc  minlogcurve function
#' @param initpars inital parameters
#' @export
fit_wafer_and_plot = function(wafer,logc,minlogc,initpars,maxit=10000){
  par(mfrow=c(4,4),oma=c(0,0,2,0))
  npars = length(initpars);
  wafer = add_forward_backward(wafer)
  estf = matrix(NA, nrow = length(unique(wafer$name)), ncol = 7)
  estb = estf
  i = unique(wafer$name)[1]
  place = 0  
  for (i in unique(wafer$name)){
    place = place + 1
    d = wafer[wafer$name==i,]
    d = trans_device(d)
    d_forward = d[d$direction == "Forward",]  
    #forwards
    datax = d_forward$VG; datay = log(d_forward$ID)
    est = optim(initpars, minlogc, datax=datax, datay=datay, control=list(maxit=maxit))
    estf[place, 1:npars] = cur_forward_pars = est$par
    estf[place, ncol(estf)] = est$value
    #################################################
    ndx = seq(-10,10,0.01)
    plot(datax,log(d_forward$ID),xlim=c(-10,10),ylim=c(-30,-5),xlab="V",ylab="I");
    lines(ndx,logc(ndx,cur_forward_pars),col="red")
    #################################################
  }
  title(paste(deparse(substitute(minlogc)),"forwards") , outer=TRUE)
  par(mfrow=c(4,4),oma=c(0,0,2,0))
  i = unique(wafer$name)[1]
  place = 0  
  for (i in unique(wafer$name)){
    place = place + 1
    d = wafer[wafer$name==i,]
    d = trans_device(d)
    #backwards
    d_backward = d[d$direction == "Backward",]
    datax = d_backward$VG; datay= log(d_backward$ID)
    est = optim(cur_forward_pars, minlogc, datax=datax, datay=datay, control=list(maxit=maxit))
    estb[place, 1:npars] = cur_backward_pars = est$par
    estb[place, ncol(estb)] = est$value
    
    #################################################
    ndx = seq(-10,10,0.01)
    plot(datax,log(d_backward$ID),xlim=c(-10,10),ylim=c(-30,-5),xlab="V",ylab="I")
    lines(ndx,logc(ndx,cur_backward_pars),col="red")
    #################################################
  }
  title(paste(deparse(substitute(minlogc)),"backwards") , outer=TRUE)
  for_params = apply(estf[, 1:npars, drop=FALSE], 2, weighted.mean, estf[,7], na.rm=TRUE)
  back_params = apply(estb[, 1:npars, drop=FALSE], 2, weighted.mean, estb[,7], na.rm=TRUE)
  results = data.frame(rbind(for_params, back_params), 
                       cost = c(mean(estf[,7]), mean(estb[,7])),
                       id = wafer$wafer_id[1], 
                       direction= c("Forward", "Backward"), stringsAsFactors = FALSE)
  rownames(results) = NULL
  class(results) = c("wafer", class(results))
  return(results)
}

#' fit_wafer_with_curve
#' @inheritParams fit_wafer_and_plot
#' @export
fit_wafer_with_curve = function(wafer,logc,minlogc,initpars,maxit=10000){
  npars = length(initpars);
  wafer = add_forward_backward(wafer)
  estf = matrix(NA, nrow = length(unique(wafer$name)), ncol = 7)
  estb = estf
  i = unique(wafer$name)[1]
  place = 0  
  for (i in unique(wafer$name)){
    place = place + 1
    d = wafer[wafer$name==i,]
    d = trans_device(d)
    d_forward = d[d$direction == "Forward",]  
    #forwards
    datax = d_forward$VG; datay = log(d_forward$ID)
    est = optim(initpars, minlogc, datax=datax, datay=datay, control=list(maxit=maxit))
    estf[place, 1:npars] = cur_forward_pars = est$par
    estf[place, ncol(estf)] = est$value
    
    #backwards
    d_backward = d[d$direction == "Backward",]
    datax = d_backward$VG; datay= log(d_backward$ID)
    est = optim(cur_forward_pars, minlogc, datax=datax, datay=datay, control=list(maxit=maxit))
    estb[place, 1:npars] = cur_backward_pars = est$par
    estb[place, ncol(estb)] = est$value
  }
  for_params = apply(estf[, 1:npars, drop=FALSE], 2, weighted.mean, estf[,7], na.rm=TRUE)
  back_params = apply(estb[, 1:npars, drop=FALSE], 2, weighted.mean, estb[,7], na.rm=TRUE)
  results = data.frame(rbind(for_params, back_params), 
                       cost = c(mean(estf[,7]), mean(estb[,7])),
                       id = wafer$wafer_id[1], 
                       direction= c("Forward", "Backward"), stringsAsFactors = FALSE)
  rownames(results) = NULL
  class(results) = c("wafer", class(results))
  return(results)
}

#' fit_wafer_with_curve_diff
#' @inheritParams fit_wafer_and_plot
#' @param diffminlogc first derivatives of minlogcurve function
#' @export
fit_wafer_with_curve_diff = function(wafer,logc,minlogc,diffminlogc,initpars,maxit=10000){
  npars = length(initpars);
  wafer = add_forward_backward(wafer)
  estf = matrix(NA, nrow = length(unique(wafer$name)), ncol = 7)
  estb = estf
  i = unique(wafer$name)[1]
  place = 0  
  for (i in unique(wafer$name)){
    place = place + 1
    d = wafer[wafer$name==i,]
    d = trans_device(d)
    d_forward = d[d$direction == "Forward",]  
    #forwards
    datax = d_forward$VG; datay = log(d_forward$ID)
    est = optim(initpars, minlogc, datax=datax, datay=datay, control=list(maxit=maxit))
    estf[place, 1:npars] = cur_forward_pars = est$par
    estf[place, ncol(estf)] = est$value
    
    #backwards
    d_backward = d[d$direction == "Backward",]
    datax = d_backward$VG; datay= log(d_backward$ID)
    est = optim(cur_forward_pars, minlogc, datax=datax, datay=datay, control=list(maxit=maxit))
    estb[place, 1:npars] = cur_backward_pars = est$par
    estb[place, ncol(estb)] = est$value
  }
  for_params = apply(estf[, 1:npars, drop=FALSE], 2, weighted.mean, estf[,7], na.rm=TRUE)
  back_params = apply(estb[, 1:npars, drop=FALSE], 2, weighted.mean, estb[,7], na.rm=TRUE)
  results = data.frame(rbind(for_params, back_params), 
                       cost = c(mean(estf[,7]), mean(estb[,7])),
                       id = wafer$wafer_id[1], 
                       direction= c("Forward", "Backward"), stringsAsFactors = FALSE)
  rownames(results) = NULL
  class(results) = c("wafer", class(results))
  return(results)
}

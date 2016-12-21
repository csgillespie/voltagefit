#Wafer validation and transformation functions.
#
#' @rdname fit_wafer
#' @export
trans_device = function(wafer) {
  wafer$ID = pmax(wafer$ID, 1e-13)
  wafer$ID = pmin(wafer$ID, 1e-3)
  wafer[wafer$VG > -5,]
}

#' @rdname fit_wafer
#' @export
validate_device = function(wafer) {
  waferf = wafer[wafer$direction == "Forward", ]
  ID = waferf[waferf$VG < -4, "ID"]
  if(any(ID > 1e-10)) return(FALSE)
  
  ID = waferf[waferf$VG > 4, "ID"]
  if(any(ID < 1e-5)) return(FALSE)
  
  waferb = wafer[wafer$direction == "Backward", ]
  ID = waferb[waferb$VG < -4, "ID"]
  if(any(ID > 1e-10)) return(FALSE)
  
  ID = waferb[waferb$VG > 4, "ID"]
  if(any(ID < 1e-5)) return(FALSE)
  return(TRUE)
}

#' @rdname fit_wafer
#' @export
trans_filter_device = function(wafer,validate=validate_device,trans=trans_device,verbose=TRUE) {
  filtered_wafer = trans(wafer)
  uqnames = unique(wafer$name)
  for(i in 1:length(uqnames)){
    if(!validate(wafer[wafer$name==uqnames[i],])) {
      filtered_wafer = filtered_wafer[filtered_wafer$name!=uqnames[i],]
      if(verbose) message(paste("Rejecting device",uqnames[i],sep=" "))
    }
  }
  filtered_wafer
}


#' Determine forward/backward curve
#' 
#' Adds a new column called `direction` indicating if the row relates
#' to forward or backward.
#' @inheritParams fit_wafer
#' @export
add_forward_backward = function(wafer) {
  dd = wafer
  dd$direction = "NULL"
  dd$direction[c(1,diff(wafer$VG)) < 0] = "Backward"  #length(diff(wafer)) = length(wafer)-1 
  dd$direction[c(1,diff(wafer$VG)) >= 0] = "Forward"  #so add c(1,..) to the front for alignment
  return(dd)
}

#' Fit curves to a wafer
#'
#' Fits curves to a wafer and returns the curve parameters and resulting
#' cost for both forward and backwards curves. Wafers can be validated and
#' transformed before fitting. Devices are fitted in parallel automatically.
#'
#' @param wafer       Data from a single wafer
#' @param trans       (Optional) Wafer ransform function
#' @param validate    (Optional) Wafer validate function
#' @param cost_func   (Optional) Cost function
#' @param dev_curve   (Optional) Model device voltage curve
#' @param initparams  (Optional) Inital parameter estimation
#' @param maxit       (Optional) Maxiumum number of iterations for use in optimiser (Default: 10000)
#' @param verbose     (Optional) Print verbose output (Default: \code{TRUE})
#' @param plot     (Optional) Plot all devices and fitted curves (Default: \code{FALSE})
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
#' data(voltage, package="voltagefit")
#' \dontrun{
#' fit_wafer(wafer6138)
#' }
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom stats lm manova vcov approx
#' @importFrom MASS mvrnorm
#' @importFrom stats coefficients optim weighted.mean
#' @importFrom graphics axis title
#' @importFrom foreach foreach %dopar% getDoParWorkers
#' @importFrom doParallel registerDoParallel
#' @importFrom minqa bobyqa
#' 
#' @export
fit_wafer = function(wafer, trans=trans_device, validate=validate_device,
                     cost_func=area_between_curves, dev_curve=curve_piecewise,
                     initparams = NULL, maxit=1e5, verbose=TRUE,plot=FALSE){
  wafer = add_forward_backward(wafer)
  wafer = trans_filter_device(wafer,validate,trans,verbose)
  uqnames = unique(wafer$name)
  
  #Default or user supplied initial parameters?
  if(is.null(initparams)) initparams = attr(dev_curve, "initparams")
  psc = rep(1, length(initparams))
  if(!is.null(attr(dev_curve, "parscale"))) psc = attr(dev_curve, "parscale")
  npars = length(initparams)
  
  #do the main fitting
  i = 1
  if(verbose) message(paste("Executing on", getDoParWorkers(), "workers."))
  estall <- foreach(i=1:length(uqnames),.combine=rbind) %dopar% {
    d = wafer[wafer$name==uqnames[i],]
    #forward
    d_forward = d[d$direction == "Forward",]
    datax = d_forward$VG
    datay = log(d_forward$ID)
    
    (est = bobyqa(initparams, cost_func, device_model=dev_curve, 
                  upper = attr(dev_curve,"for_upper"),
                  lower = attr(dev_curve,"for_lower"),
                  datax=datax, datay=datay, control = list(maxfun=maxit, rhobeg=0.1)))
    
    cur_forward_pars = est$par
    cur_forward_value = est$fval
    #backward
    d_backward = d[d$direction == "Backward",]
    datax = d_backward$VG
    datay= log(d_backward$ID)
    est = bobyqa(attr(dev_curve,"back_upper"), cost_func, device_model=dev_curve, 
                 datax=datax, datay=datay, 
                 upper = attr(dev_curve,"back_upper"),
                 lower = attr(dev_curve,"back_lower"),
                 control = list(maxfun=maxit, rhobeg=0.1))
    cur_backward_pars = est$par
    cur_backward_value = est$fval
    
    #this line looks like it's not doing anything, but it is
    #returning rows to the parallel backend which are then rbind and returned
    c(cur_forward_pars,cur_forward_value,cur_backward_pars,cur_backward_value)
  }
  
  if(plot == TRUE){
    for(i in 1:length(uqnames)){
      d = wafer[wafer$name==uqnames[i],]
      d_forward = d[d$direction == "Forward",]
      datax = d_forward$VG
      datay = log(d_forward$ID)
      plot(datax,datay,t='l')
      lines(datax,dev_curve(datax,estall[i, 1:npars, drop=FALSE]),col=2)
      d_backward = d[d$direction == "Backward",]
      datax = d_backward$VG
      datay = log(d_backward$ID)
      plot(datax,datay,t='l')
      lines(datax,dev_curve(datax,estall[i, (npars+2):(2*npars+1), drop=FALSE]),col=2)
    }
  }
  
  #Build results data.frame
  for_params  = apply(estall[, 1:npars, drop=FALSE], 2, 
                      weighted.mean, 
                      1/estall[,npars+1, drop=FALSE], na.rm=TRUE)
  back_params = apply(estall[, (npars+2):(2*npars+1), drop=FALSE], 2, 
                      weighted.mean, 
                      1/estall[,2*npars+2, drop=FALSE], na.rm=TRUE)
  
  results = data.frame(rbind(for_params, back_params), 
                       cost = c(mean(estall[,npars+1]), mean(estall[,2*npars+2])),
                       id = wafer$wafer_id[1], 
                       direction= c("Forward", "Backward"), stringsAsFactors = FALSE)
  rownames(results) = NULL
  
  attr(results,"wafer_back_forward") <- wafer
  attr(results,"dev_curve") <- dev_curve
  class(results) = c("wafer", class(results))
  return(results)
}
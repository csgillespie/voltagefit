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
  wafer = wafer[wafer$direction == "Forward", ]
  ID = wafer[wafer$VG < -5, "ID"]
  if(any(ID > 1e-9)) return(FALSE)
  
  ID = wafer[wafer$VG > 5, "ID"]
  if(any(ID < 1e-5)) return(FALSE)
  return(TRUE)
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


#' Fit curves to multiple wafers
#'
#' Fits curves to multiple wafers in a directory, using \code{\link{fit_wafer}}, 
#' returning the parameters and cost for both forward and backwards curves.
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
fit_all = function(path, maxit=10000, verbose=TRUE,plot=FALSE){
  files = list.files(path = path, pattern = ".rds") ## get files to read
  if(verbose) message("Files found: ", paste(files,collapse=" "))
  
  for(i in 1:length(files)){
    wafer = readRDS(file.path(path, files[i])) ## read a file/wafer
    if(verbose) message("Reading: ", files[i])
    fit = fit_wafer(wafer, maxit=maxit, verbose=verbose,plot=plot) ## fit model to that wafer
    
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


#' Fit curves to a wafer
#'
#' Fits curves to a wafer and returns the curve parameters and resulting
#' cost for both forward and backwards curves. Wafers can be validated and
#' transformed before fitting. Devices are fitted in parallel automatically, 
#' however, note that \code{plot=TRUE} temporarily disables this parallelisation.
#'
#' @param wafer       Data from a single wafer
#' @param trans       (Optional) Wafer ransform function
#' @param validate    (Optional) Wafer validate function
#' @param cost_func   (Optional) Cost function
#' @param dev_curve   (Optional) Model device voltage curve
#' @param initparams  (Optional) Inital parameter estimation
#' @param maxit       (Optional) Maxiumum number of iterations for use in optimiser (Default: 10000)
#' @param verbose     (Optional) Print verbose output (Default: \code{TRUE})
#' @param plot        (Optional) Plot each devices with its fitted model (Default: \code{FALSE})
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
#' @importFrom stats lm manova vcov approx
#' @importFrom MASS mvrnorm
#' @importFrom stats coefficients optim weighted.mean
#' @importFrom graphics axis title
#' @importFrom foreach foreach %dopar% getDoParWorkers
#' @importFrom doParallel registerDoParallel
#' 
#' @export
fit_wafer = function(wafer, trans=trans_device, validate=validate_device,
                     cost_func=area_between_curves, dev_curve=curve_4BARO, 
                     initparams = NULL, maxit=10000,
                     verbose=TRUE, plot=FALSE){
  wafer = add_forward_backward(wafer)
  uqnames = unique(wafer$name)
  
  #Default or user supplied initial parameters?
  if(is.null(initparams)) initparams = attr(dev_curve,"initparams")
  npars = length(initparams);
  
  #Prepare for plotting
  #NOTE: Here we turn off parallel so that R can plot to the same output for each device
  if(plot){
    noWorkers = getDoParWorkers()
    registerDoParallel(1)
    par(mfrow=c(4,4))
    if(verbose) message("Plotting is enabled")
  }
  
  #do the main fitting
  i = 0
  if(verbose) message(paste("Executing on ",getDoParWorkers()," workers.",sep=" "))
  estall<-foreach(i=1:length(uqnames),.combine=rbind) %dopar% {
    d = trans(wafer[wafer$name==uqnames[i],])
    if(validate(d)) {
      #forward
      d_forward = d[d$direction == "Forward",]
      datax = d_forward$VG
      datay = log(d_forward$ID)
      est = optim(initparams, cost_func, attr(cost_func,"derivs"), device_model=dev_curve, 
                  datax=datax, datay=datay, control=list(maxit=maxit))
      cur_forward_pars = est$par
      cur_forward_value = est$value
      if(plot){
        ndx = seq(-10,10,0.01)
        plot(datax,log(d_forward$ID),xlim=c(-10,10),ylim=c(-30,-5),xlab="V",ylab="I");
        lines(ndx,dev_curve(ndx,cur_forward_pars),col="red")
        title(paste(uqnames[i]," - Forward",sep=""))
      }
      #backward
      d_backward = d[d$direction == "Backward",]
      datax = d_backward$VG
      datay= log(d_backward$ID)
      est = optim(cur_forward_pars, cost_func, attr(cost_func,"derivs"), device_model=dev_curve, 
                  datax=datax, datay=datay, control=list(maxit=maxit))
      cur_backward_pars = est$par
      cur_backward_value = est$value
      if(plot){
        ndx = seq(-10,10,0.01)
        plot(datax,log(d_backward$ID),xlim=c(-10,10),ylim=c(-30,-5),xlab="V",ylab="I")
        lines(ndx,dev_curve(ndx,cur_backward_pars),col="red")
        title(paste(uqnames[i]," - Backward",sep=""))
      }
      #this line looks like it's not doing anything, but it is
      #returning rows to the parallel backend which are then rbind and returned
      c(cur_forward_pars,cur_forward_value,cur_backward_pars,cur_backward_value)
    }
  }
  
  #Turn parallel back on if plotting
  if(plot) registerDoParallel(noWorkers) 
  
  #Build results data.frame
  for_params  = apply(estall[, 1:npars, drop=FALSE], 2, weighted.mean, estall[,npars+1], na.rm=TRUE)
  back_params = apply(estall[, (npars+2):(2*npars+1), drop=FALSE], 2, weighted.mean, estall[,2*npars+2], na.rm=TRUE)
  
  results = data.frame(rbind(for_params, back_params), 
                       cost = c(mean(estall[,npars+1]), mean(estall[,2*npars+2])),
                       id = wafer$wafer_id[1], 
                       direction= c("Forward", "Backward"), stringsAsFactors = FALSE)
  rownames(results) = NULL
  
  class(results) = c("wafer", class(results))
  return(results)
}

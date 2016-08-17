#' Fit logcurves to a wafer
#'
#' Fits logcurves to a wafer and returns the curve parameters and resulting
#' cost for both forward and backwards curves. It is assumed that any wafer
#' entering this function has been pre-filtered. See the function "filtered"
#' for details of how wafers are filtered.
#'
#' @param wafer     Data from a single wafer.
#' @param iterlim   Maxiumum number of iterations to use in nlm (Default: 2000).
#' @param id        WaferID (Default: A random string is generated as the ID).
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
#'
#' @examples
#' fit_wafer(wafer3737)
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom stats lm manova nlm vcov
#' @importFrom MASS mvrnorm
#' 
#' @export
fit_wafer = function(wafer, iterlim=2000, id=NULL){
  if (is.null(id)){
    id = stri_rand_strings(1, 5)  ## check wafer has an id, if not generate one
  } else {
    id = gsub(".rds","",id)
  }
  
  ## get total observations on a device and total number in the forward pass
  tot_obs = length(wafer[wafer$name==unique(wafer$name)[1],]$VG) 
  tot_f = which(diff(wafer[wafer$name==unique(wafer$name)[1],]$VG) < 0)[1]
  
  estf = matrix(0, nrow = dim(wafer)[1] / tot_obs, ncol = 7)
  estb = estf

  place = 0  
  for (i in unique(wafer$name)){ ## iterate through each device on the wafer
    place = place + 1
    d = wafer[wafer$name==i,]  ## get the data for the device
    
    d_forward = d[1:tot_f,]  ## get the forward pass
    d_backward = d[tot_f:tot_obs,]  ## get the backward pass
    
    #forwards
    ## fit the model to the forward pass, fitted to transformed data to improve fit
    m.nlmf = nlm(min_logcurve, c(1.0, 1.4, -3, 4, -1.5, 0.3), d_forward$VG,
                 max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)), iterlim = iterlim) 
    estf[place,1:6] = m.nlmf$estimate  ## store parameters 
    ## warning if the model doesn't fit within the specified iterations 
    if(m.nlmf$iterations==iterlim) message(paste("Max reached for forward, device ",i))
    ## calculate the final cost and store
    estf[place,7] = min_logcurve(m.nlmf$estimate, d_forward$VG,
                               max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)))
    
    #backwards (as above but for backwards pass)
    m.nlmb = nlm(min_logcurve_back, c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01), d_backward$VG,
                 max(log(abs(d_backward$ID))) / log(abs(d_backward$ID)), m.nlmf$estimate, iterlim = iterlim)
    estb[place,1:6] = m.nlmb$estimate
    if(m.nlmb$iterations==iterlim) message(paste("Max reached for backward, device ",i))
    estb[place,7] = min_logcurve_back(m.nlmb$estimate, d_backward$VG,
                                max(log(abs(d_backward$ID))) / log(abs(d_backward$ID)), m.nlmf$estimate)
  }
  ## combine into data.frame
  forward = data.frame(id = id, name = unique(wafer$name), max = max(log(abs(wafer$ID))), 
                        cost = estf[,7], direction = "Forward", estf[,1:6])
  backward = data.frame(id = id, name = unique(wafer$name), max = max(log(abs(wafer$ID))), 
                        cost = estb[,7], direction = "Backward", estb[,1:6])
  
  results = rbind(forward, backward)
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
fit_all = function(path, iterlim=2000){
  files = list.files(path = path, pattern = ".rds") ## get files to read
  message("Files found: ",paste(files,collapse=" "))
  
  for(i in 1:length(files)){
    wafer = readRDS(file.path(path, files[i])) ## read a file/wafer
    message("Reading: ",files[i])
    fit = fit_wafer(wafer, iterlim, files[i]) ## fit model to that wafer
    
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

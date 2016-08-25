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


#' Fit logcurves to a wafer
#'
#' Fits logcurves to a wafer and returns the curve parameters and resulting
#' cost for both forward and backwards curves. It is assumed that any wafer
#' entering this function has been pre-filtered. See the function "filtered"
#' for details of how wafers are filtered.
#'
#' @param wafer     Data from a single wafer.
#' @param maxit   Maxiumum number of iterations to use in nlm (Default: 10000).
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
fit_wafer = function(wafer, maxit=10000, verbose=TRUE){
  wafer = add_forward_backward(wafer)

  estf = matrix(0, nrow = length(unique(wafer$name)), ncol = 7)
  estb = estf
  i = unique(wafer$name)[1]
  place = 0  
  cur_forward_pars = cur_backward_pars = c(1.0, 1.4, -3, 4, -1.5, 0.3)
  cur_forward_pars = cur_backward_pars = c(-9.1123, -15.5432, 1.1966,
                                           0.6834, -0.1843, 0.1418)
  for (i in unique(wafer$name)){ ## iterate through each device on the wafer
    if(verbose) message(i)
    place = place + 1
    d = wafer[wafer$name==i,]  ## get the data for the device
      d_forward = d[d$direction == "Forward",]  

    #forwards
    datax = d_forward$VG; datay= log(pmax(abs(d_forward$ID), 1e-13))
    est = optim(cur_forward_pars, min_logcurve, datax=datax, datay=datay, control=list(maxit=maxit))
    estf[place, 1:6] = cur_forward_pars = est$par
    
   # if(m.nlmf$iterations==iterlim) message(paste("Max reached for forward, device ", i))
    estf[place, ncol(estf)] = est$value

    d_backward = d[d$direction == "Backward",]
    datax = d_backward$VG; datay= log(pmax(abs(d_backward$ID), 1e-13))
    est = optim(cur_forward_pars, min_logcurve, datax=datax, datay=datay, control=list(maxit=maxit))
    estb[place, 1:6] = cur_backward_pars = est$par
    #if(m.nlmb$iterations==iterlim) message(paste("Max reached for backward, device ",i))
    estb[place, ncol(estb)] = est$value
  }
  
  for_params = apply(estf[, 1:6, drop=FALSE], 2, weighted.mean, estf[,7], na.rm=TRUE)
  back_params = apply(estb[, 1:6, drop=FALSE], 2, weighted.mean, estb[,7], na.rm=TRUE)
  
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

#' Cost functions
#'
#' Cost functions are used during parameter estimation, and in general quantifiy
#' the difference between a given model and true data, for a certain set of model parameters.
#'
#' @param device_model  the model voltage curve function
#' @param param         list of parameters for the model voltage curve function
#' @param datax         x values resulting from a measurement of a device
#' @param datay         y values resulting from a measurement of a device
#' 
#' @return A value representing the "cost" associated with the given model and parameters.
#'
#' @export
sum_residuals = function(param, datax, datay, device_model){
  fity = device_model(datax, param)
  res = (fity-datay)^2
  sum(res[!is.nan(res)])
}

#' @rdname sum_residuals
#' @export
sum_log_residuals = function(param, datax, datay, device_model){
  fity = device_model(datax, param)
  res = 1/log((fity-datay)^2)
  sum(res[!is.nan(res)])
}

# Interpolatated residuals
#' @rdname sum_residuals
#' @export
interp_residuals = function(param, datax, datay, device_model){
  interp = approx(datax,datay,n = length(datax))
  fity = device_model(interp$x, param)
  res = (fity-datay)^2
  sum(res[!is.nan(res)])
}

# Area between curves
# 
# Derived using linear interpolation on the model curve and datay values, 
# interpolated between datax points.
#  
#' @rdname sum_residuals
#' @export
area_between_curves = function(param, datax, datay, device_model){
  fity = device_model(datax, param)
  areas = (0.5*diff(datax)*(datay[1:length(datax)-1] + datay[2:length(datax)] 
                        - fity[1:length(datax)-1] - fity[2:length(datax)]))^2
  sum(areas[!is.nan(areas)])
}
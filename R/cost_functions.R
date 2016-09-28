#' @rdname interp_residuals
#' @inheritParams  interp_residuals
#' @export
cost_abc_derivs = function(param,device_model,datax,datay){
  #get correct derivatives for device model in use
  curve_derivs_func = attr(device_model,"derivs")
  curve_derivs = curve_derivs_func(param,datax)
  #get fitted values for current model/params
  fity = device_model(datax, param)
  #get derivatives
  derivs = 0.5*(diff(datax)^2)*(datay[1:length(datax)-1] + datay[2:length(datax)] - 
                                  fity[1:length(datax)-1] - fity[2:length(datax)])*(-curve_derivs[,1:length(datax)-1]-curve_derivs[,2:length(datax)])
  rowSums(derivs)
}



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
#' @rdname interp_residuals
#' @export
area_between_curves = function(param, datax, datay, device_model){
  fity = device_model(datax, param)
  areas = (0.5*diff(datax)*(datay[1:length(datax)-1] + datay[2:length(datax)] 
                        - fity[1:length(datax)-1] - fity[2:length(datax)]))^2
  sum(areas[!is.nan(areas)])
}
attr(area_between_curves,"derivs") <- cost_abc_derivs
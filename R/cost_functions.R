#' Interpolated residuals cost function
#'
#' Cost function to be used during parameter estimation, quantifying the difference 
#' between a given model and the true data for a certain set of model parameters.
#' The cost defined by this function is based on a sum of residuals over the datax 
#' values given Interpolation is used to create evenly spaced points as the input 
#' datax values may have a varying density of points thoughtout. The first derivatives 
#' with respect to each parameter can also be calculated. 
#' 
#' @param param         a list consisting of parameters for the model curve.
#' @param datax         a range of values over which the voltage curve is evaluated.
#' @param datay         the true voltage values for the datax values given.
#' @param device_model  the model voltage curve function.
#' 
#' @export
interp_residuals = function(param, datax, datay, device_model){
  interp = approx(datax,datay,n = length(datax))
  fity = device_model(interp$x, param)
  res = (fity-interp$y)^2
  sum(res)
}

#' Area between curves cost function
#'
#' Cost function to be used during parameter estimation, quantifying the difference 
#' between a given model and the true data for a certain set of model parameters.
#' The cost defined by this function is based on the total area between the curves generated
#' by linear interpolation on the model curve and true values, interpolated between the datax 
#' points given. The first derivatives with respect to each parameter can also be calculated. 
#' 
#' @param param         a list consisting of parameters for the model curve.
#' @param datax         a range of values over which the voltage curve is evaluated.
#' @param datay         the true voltage values for the datax values given.
#' @param device_model  the model voltage curve function.
#' 
#' @export
area_between_curves = function(param, datax, datay, device_model){
  fity = device_model(datax, param)
  areas = (0.5*diff(datax)*(datay[1:length(datax)-1] + datay[2:length(datax)] 
                        - fity[1:length(datax)-1] - fity[2:length(datax)]))^2
  sum(areas)
}
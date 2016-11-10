#' Tanh model voltage curve
#'
#' Model voltage curve based on a tanh sigmoidal shape. The first derivatives with 
#' respect to each parameter can also be calculated. 
#' 
#' @param x             a range of values for which the model curve is evaluated
#' @param param         list consisting of 4 parameters for the curve function:
#' \describe{
#'      \item{theta_1}{Value of y at center of the curve}
#'      \item{theta_2}{Distance from the center of the curve to the top/bottom}
#'      \item{theta_3}{A measure of the steepness of the curve}
#'      \item{theta_4}{Value of x shift applied to the data}
#'   }
#' 
#' @return A range of values, corresponding to the value of the tanh curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_tanh = function(x, param){
  param[1] + param[2]*tanh((x-param[4])/param[3])
}
attr(curve_tanh,"initparams") <- c(-30,25,0.5,0)
attr(curve_tanh,"name") <- "curve_tanh"

#' 4PL model voltage curve
#'
#' Model voltage curve based on the 4PL model. The first derivatives with 
#' respect to each parameter can also be calculated. 
#' NOTE: Here the data is shifted >> 0 in x, as the raw curve is not designed 
#' to handle x values of less than zero.
#' 
#' @inheritParams  curve_tanh
#' @param param         list consisting of 4 parameters for the curve function:
#' \describe{
#'      \item{theta_1}{Value of y at the curve minimum}
#'      \item{theta_2}{Value of y at the curve maximum}
#'      \item{theta_3}{Value of x (+100) at the point of inflection}
#'      \item{theta_4}{A measure of the steepness of the curve}
#'   }
#' 
#' @return A range of values, corresponding to the value of the 4PL curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_4PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+100)/param[3])^param[2])
}
attr(curve_4PL,"initparams") <- c(-30,5,100,-10)
attr(curve_4PL,"name") <- "curve_4PL"

#' 5PL model voltage curve
#'
#' Model voltage curve based on the 5PL model. The first derivatives with 
#' respect to each parameter can also be calculated. 
#' NOTE: Here the data is shifted >> 0 in x, as the raw curve is not designed 
#' to handle x values of less than zero.
#' 
#' @inheritParams  curve_tanh
#' @param param         list consisting of 5 parameters for the curve function:
#' \describe{
#'      \item{theta_1}{Value of y at the curve minimum}
#'      \item{theta_2}{Value of y at the curve maximum}
#'      \item{theta_3}{Value of x (+100) at the point of inflection}
#'      \item{theta_4}{A measure of the steepness of the curve}
#'      \item{theta_5}{A measure of the asymmetry}
#'   }
#' 
#' @return A range of values, corresponding to the value of the 5PL curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_5PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+100)/param[3])^param[2])^param[5]
}
attr(curve_5PL,"initparams") <- c(-30,5,100,-10,5)
attr(curve_5PL,"name") <- "curve_5PL"

#' 4BARO model voltage curve
#'
#' Model voltage curve based on the 4BARO model. The first derivatives with 
#' respect to each parameter can also be calculated. 
#' 
#' @inheritParams  curve_tanh
#' @param param         list consisting of 4 parameters for the curve function:
#' \describe{
#'      \item{theta_1}{Value of y at the curve minimum}
#'      \item{theta_2}{The range of response. Note: (theta_1+theta_2) gives 
#'      the value of y at the curve minimum}
#'      \item{theta_3}{A measure of the steepness of the curve}
#'      \item{theta_4}{Value of x shift applied to the data}
#'   }
#' 
#' @return A range of values, corresponding to the value of the 4BARO curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_4BARO = function(x, param){
  param[1]+(param[2]/(1+exp(param[3]*(x-param[4]))))
}
attr(curve_4BARO,"initparams") <- c(-10,-30,5,0)
attr(curve_4BARO,"name") <- "curve_4BARO"

#' 5BARO model voltage curve
#'
#' Model voltage curve based on the 5BARO model. The first derivatives with 
#' respect to each parameter can also be calculated. 
#' 
#' @inheritParams  curve_tanh
#' @param param         list consisting of 5 parameters for the curve function:
#' \describe{
#'      \item{theta_1}{Value of y at the curve minimum}
#'      \item{theta_2}{The range of response. Note: (theta_1+theta_2) gives 
#'      the value of y at the curve minimum}
#'      \item{theta_3}{A measure of the steepness of the curve}
#'      \item{theta_4}{Value of x shift applied to the data}
#'      \item{theta_5}{A measure of the asymmetry}
#'   }
#' 
#' @return A range of values, corresponding to the value of the 5BARO curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_5BARO = function(x, param){
  cfbar = (2*param[3]*param[5])/abs(param[3]+param[5])
  fx = 1/(1+exp(-cfbar*(x-param[4])))
  param[1] + param[2] / (1 + fx*exp(param[3]*(x-param[4])) + (1-fx)*exp(param[5]*(x-param[4])))
}
attr(curve_5BARO,"initparams") <- c(-10,-30,5,0,5)
attr(curve_5BARO,"name") <- "curve_5BARO"

#'Piecewise power model voltage curve
#'
#'Model voltage curve based on a power fit. Two pmax are used. pmax(0,x) 
#'removes the -ve x part of the curve. pmax(log(1e-13),v) is used to cap 
#'the minimum value to log(1e-13).
#'
#' @inheritParams  curve_tanh
#' @param param         list consisting of 4 parameters for the curve function:
#' 
#' @return A range of values, corresponding to the value of the curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_power = function(x, param){
  v = param[1] + param[2]*(pmax(0,x+param[3]))^(param[4])
  pmax(param[5],v)
}
attr(curve_power,"initparams") <- c(-3.45,-14,0.175,-0.5,-30)
attr(curve_power,"name") <- "curve_power"
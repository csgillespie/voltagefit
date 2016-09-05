#' Model device voltage curves
#'
#' Functions implementing voltage curves. Several curve types are available 
#' for use. Each type of curve can be varied through a list of parameters.
#'
#' @param x             a range of values for which the model curve is evaluated
#' @param param         list of parameters for the model curve function
#' 
#' @return A range of values, corresponding to the value of the model curve for 
#' the given parameters, over the provided range of x values. 
#'
#' @export
curve_exp_poly = function(x, param){
  param[1] + param[2] / (1 + exp(param[3] + param[4]*x + param[5]*x^2 + param[6]*x^3))
}
# sample initparams = c(-9.1123, -15.5432, 1.1966,0.6834, -0.1843, 0.1418)

# param_1 = value of y at center of the curve
# param_2 = distance from the center of the curve to the top/bottom
# param_3 = a measure of the steepness of the curve
# param_4 = x shift
#
# sample initparams = c(-10,10,0.5,0)
#
#' @rdname curve_exp_poly
#' @export
curve_tanh = function(x, param){
  param[1] + param[2]*tanh((x-param[4])/param[3])
}

# param_1 = the minimum value
# param_2 = the maximum value
# param_3 = the point of inflection(+100)
# param_4 = a measure of the steepness of the curve
# NOTE: Here the data is shifted >> 0, as the raw curve is not designed to handle x values of less than zero.
#
# sample initparams = c(-30,5,100,-10)
#
#' @rdname curve_exp_poly
#' @export
curve_4PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+100)/param[3])^param[2])
}

# param_1 = the minimum value
# param_2 = the maximum value
# param_3 = the point of inflection(+100)
# param_4 = a measure of the steepness of the curve
# param_5 = a measure of the asymmetry
# NOTE: Here the data is shifted >> 0, as the raw curve is not designed to handle x values of less than zero.
#
# sample initparams = c(-30,5,100,-10,5)
#
#' @rdname curve_exp_poly
#' @export
curve_5PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+100)/param[3])^param[2])^param[5]
}

# param_1 = the minimum value
# param_2 = the range of response. Note: (P1+P2) gives the minimum value
# param_3 = a measure of the steepness of the curve
# param_4 = x shift
#
# sample initparams = c(-10,-30,5,0)
#
#' @rdname curve_exp_poly
#' @export
curve_4BARO = function(x, param){
  param[1]+(param[2]/(1+exp(param[3]*(x-param[4]))))
}

# param_1 = the minimum value
# param_2 = the range of response. Note: (P1+P2) gives the minimum value
# param_3 = a measure of the steepness of the curve
# param_4 = x shift
# param_5 = a measure of curve asymmetry
#
# sample initparams = c(-10,-30,5,0,5)
#
#' @rdname curve_exp_poly
#' @export
curve_5BARO = function(x, param){
  cfbar = (2*param[3]*param[5])/abs(param[3]+param[5])
  fx = 1/(1+exp(-cfbar*(x-param[4])))
  param[1] + param[2] / (1 + fx*exp(param[3]*(x-param[4])) + (1-fx)*exp(param[5]*(x-param[4])))
}
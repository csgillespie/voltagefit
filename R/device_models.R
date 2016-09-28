#' @rdname curve_tanh
#' @inheritParams  curve_tanh
#' @export
curve_tanh_derivs = function(param,x){
  rsec = (x - param[3])/param[4]
  srsec = 1/cosh(rsec)
  dzdt1 = x*0+1
  dzdt2 = tanh(rsec)
  dzdt3 = - (param[2]*srsec^2)/param[4]
  dzdt4 = - (param[2]*(x-param[3])*srsec^2)/(param[4]^2)
  rbind(dzdt1,dzdt2,dzdt3,dzdt4)
}

#' @rdname curve_4PL
#' @inheritParams  curve_4PL
#' @export
curve_4PL_derivs = function(param,x){
  rsec = ((x + 100)/param[3])^param[2]
  dzdt1 = 1/(rsec+1)
  dzdt2 = (param[1]-param[4])*rsec*log((x+100)/param[3])/(rsec + 1)
  dzdt3 = param[2]*(param[1]-param[4])*rsec/(param[3]*(rsec+1))
  dzdt4 = 1 - 1/(rsec+1)
  rbind(dzdt1,dzdt2,dzdt3,dzdt4)
}

#' @rdname curve_5PL
#' @inheritParams  curve_5PL
#' @export
curve_5PL_derivs = function(param,x){
  rsec = ((x + 100)/param[3])^param[2]
  dzdt1 = 1/(rsec+1)^param[5]
  dzdt2 = -(param[1]-param[4])*param[5]*rsec*(rsec+1)^(-param[5]-1)*log((x+100)/param[3])
  dzdt3 = param[2]*(param[1]-param[4])*param[5]*rsec*(rsec+1)^(-param[5]-1)/param[3]
  dzdt4 = 1-1/((rsec+1)^param[5])
  dzdt5 = -(param[1]-param[4])*log(rsec+1)/((rsec+1)^param[5])
  rbind(dzdt1,dzdt2,dzdt3,dzdt4,dzdt5)
}

#' @rdname curve_4BARO
#' @inheritParams  curve_4BARO
#' @export
curve_4BARO_derivs = function(param,x){
  rsec = exp(param[3]*(x-param[4]))
  dzdt1 = 0*x+1
  dzdt2 = 1/(rsec+1)
  dzdt3 = -param[2]*(x-param[4])*rsec/((rsec+1)^2)
  dzdt4 = param[2]*param[3]*rsec/((rsec+1)^2)
  rbind(dzdt1,dzdt2,dzdt3,dzdt4)
}

#' @rdname curve_5BARO
#' @inheritParams  curve_5BARO
#' @export
curve_5BARO_derivs = function(param,x){
  cfbar = (2*param[3]*param[5])/abs(param[3]+param[5])
  fx = 1/(1+exp(-cfbar*(x-param[4])))
  rsec5 = exp(param[5]*(x-param[4]))
  rsec3 = exp(param[3]*(x-param[4]))
  
  dzdt1 = 0*x + 1
  dzdt2 = 1/((1-fx)*rsec5 + fx*rsec3+1)
  dzdt3 = - (param[2]*fx*(x-param[4])*rsec3)/(((1-fx)*rsec5 + fx*rsec3+1)^2)
  dzdt4 = - (param[2]*(-param[5]*(1-fx)*rsec5 - param[3]*fx*rsec3))/(((1-fx)*rsec5 + fx*rsec3+1)^2)
  dzdt5 = - (param[2]*(1-fx)*(x-param[4])*rsec5)/(((1-fx)*rsec5 + fx*rsec3+1)^2)
  rbind(dzdt1,dzdt2,dzdt3,dzdt4,dzdt5)
}



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
attr(curve_tanh,"derivs") <- curve_tanh_derivs
attr(curve_tanh,"initparams") <- c(-10,10,0.5,0)

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
attr(curve_4PL,"derivs") <- curve_4PL_derivs

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
attr(curve_5PL,"derivs") <- curve_5PL_derivs

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
attr(curve_4BARO,"derivs") <- curve_4BARO_derivs

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
attr(curve_5BARO,"derivs") <- curve_5BARO_derivs
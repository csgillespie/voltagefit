cost_fun = function(x,y){
  (x-y)^2
}

#Original Colin's curve
logcurve_colin = function(x, param){
  param[1] + param[2] / (1 + exp(param[3] + param[4]*x + param[5]*x^2 + param[6]*x^3))
}
min_logcurve_colin = function(param, datax, datay){
  z = logcurve_colin(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
#diffs
#f(a,b,c,d,e,f,x) := a + b / (1 + exp(c + d*x + e*x^2 + f*x^3));
#factor(diff(f(a,b,c,d,e,f,x),a));
#factor(diff(f(a,b,c,d,e,f,x),b));
#factor(diff(f(a,b,c,d,e,f,x),c));
#factor(diff(f(a,b,c,d,e,f,x),d));
#factor(diff(f(a,b,c,d,e,f,x),e));
#factor(diff(f(a,b,c,d,e,f,x),f));
fit_wafer_and_plot(wafer3737, logcurve_colin, min_logcurve_colin, c(-9.1123, -15.5432, 1.1966,0.6834, -0.1843, 0.1418))


#Three parameter tanh curve
#param[1] = value at x=0
#param[2] = distance from center of curve to top/bottom
#param[3] = a measure of the steepness of the curve
#This one isn't very good, I leave it only to remind that it was tried.
logcurve_tanh = function(x, param){
  param[1] + param[2]*tanh((x-param[4])/param[3])
}
min_logcurve_tanh = function(param, datax, datay){
  z = logcurve_tanh(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
fit_wafer_and_plot(wafer3737, logcurve_tanh, min_logcurve_tanh, c(-10,10,0.5,0))

#Four Parameter Logistic Curve (4PL)
#param[1] = the minimum value
#param[2] = the maximum value
#param[3] = the point of inflection(+100)
#param[4] = a measure of the steepness of the curve
#NOTE: Here I have shifted the data >> 0, the raw curve acts strange near zero.
logcurve_4PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+100)/param[3])^param[2])
}
min_logcurve_4PL = function(param, datax, datay){
  z = logcurve_4PL(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
fit_wafer_and_plot(wafer3737, logcurve_4PL, min_logcurve_4PL, c(-30,5,100,-10))

#Five Parameter Logistic Curve (5PL)
#param[1] = the minimum value
#param[2] = the maximum value
#param[3] = the point of inflection(+100)
#param[4] = a measure of the steepness of the curve
#param[5] = a measure of asymmetry
#NOTE: Again I have shifted the data >> 0, the raw curve acts strange near zero.
logcurve_5PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+20)/param[3])^param[2])^param[5]
}
min_logcurve_5PL = function(param, datax, datay){
  z = logcurve_5PL(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
fit_wafer_and_plot(wafer3737, logcurve_5PL, min_logcurve_5PL, c(-30,5,20,-10,1))

#Four Parameter BARO Curve (4BARO)
#param[1] = maximum value
#param[2] = range of response. Note: (P1+P2) gives the minimum value
#param[3] = a measure of the steepness of the curve
#param[4] = x shift
logcurve_4BARO = function(x, param){
  param[1]+(param[2]/(1+exp(param[3]*(x-param[4]))))
}
min_logcurve_4BARO = function(param, datax, datay){
  z = logcurve_4BARO(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
fit_wafer_and_plot(wafer3737, logcurve_4BARO, min_logcurve_4BARO, c(-10,-20,5,0))

#Five Parameter BARO Curve (5BARO)
#param[1] = maximum value
#param[2] = minimum
#param[3] = a measure of the steepness of the curve
#param[4] = x shift
#param[5] = a measure of asymmetry
logcurve_BARO5 = function(x, param){
  cfbar = (2*param[3]*param[5])/abs(param[3]+param[5])
  fx = 1/(1+exp(-cfbar*(x-param[4])))
  param[1] + param[2] / (1 + fx*exp(param[3]*(x-param[4])) + (1-fx)*exp(param[5]*(x-param[4])))
}
min_logcurve_BARO5 = function(param, datax, datay){
  z = logcurve_BARO5(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
fit_wafer_and_plot(wafer3737, logcurve_BARO5, min_logcurve_BARO5, c(-10,-30,5,0,5))


#HERE BE DRAGONS...
##The voltage x values in the forward curves are unevenly distributed, 
##so least squares emphasises the fit in high density areas.
##This uses interpolation to make the x points evenly distributed before fitting,
##making the fit "look" better in the high voltage tail.
##
##I am not sure if this is good stats or not, probably not...
##Is there a proper way to do this?
##-G
min_logcurve_BARO5I = function(param, datax, datay){
  interp = approx(datax,datay,n = length(datax))
  z = logcurve_BARO5(interp$x, param)
  res = cost_fun(z, interp$y) 
  sum(res[!is.nan(res)])
}
fit_wafer_and_plot(wafer3737, logcurve_BARO5, min_logcurve_BARO5I, c(-10,-30,5,0,5))
#fit_wafer_and_plot(wafer4465, logcurve_BARO5, min_logcurve_BARO5I, c(-10,-30,5,0,5))


## ---- echo=FALSE, results="hide"-----------------------------------------
library(voltagefit)
cost_fun = function(x,y){
  (x-y)^2
}
logcurve_colin = function(x, param){
  param[1] + param[2] / (1 + exp(param[3] + param[4]*x + param[5]*x^2 + param[6]*x^3))
}
min_logcurve_colin = function(param, datax, datay){
  z = logcurve_colin(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
logcurve_tanh = function(x, param){
  param[1] + param[2]*tanh((x-param[4])/param[3])
}
min_logcurve_tanh = function(param, datax, datay){
  z = logcurve_tanh(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
logcurve_4PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+100)/param[3])^param[2])
}
min_logcurve_4PL = function(param, datax, datay){
  z = logcurve_4PL(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
logcurve_5PL = function(x, param){
  param[4]+ (param[1]-param[4])/(1+((x+20)/param[3])^param[2])^param[5]
}
min_logcurve_5PL = function(param, datax, datay){
  z = logcurve_5PL(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
logcurve_4BARO = function(x, param){
  param[1]+(param[2]/(1+exp(param[3]*(x-param[4]))))
}
min_logcurve_4BARO = function(param, datax, datay){
  z = logcurve_4BARO(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}
logcurve_5BARO = function(x, param){
  cfbar = (2*param[3]*param[5])/abs(param[3]+param[5])
  fx = 1/(1+exp(-cfbar*(x-param[4])))
  param[1] + param[2] / (1 + fx*exp(param[3]*(x-param[4])) + (1-fx)*exp(param[5]*(x-param[4])))
}
min_logcurve_5BARO = function(param, datax, datay){
  z = logcurve_5BARO(datax, param)
  res = cost_fun(z, datay) 
  sum(res[!is.nan(res)])
}

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer_and_plot(wafer3737, logcurve_colin, min_logcurve_colin, c(-9.1123, -15.5432, 1.1966,0.6834, -0.1843, 0.1418))

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer_and_plot(wafer3737, logcurve_tanh, min_logcurve_tanh, c(-10,10,0.5,0))

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer_and_plot(wafer3737, logcurve_4PL, min_logcurve_4PL, c(-30,5,100,-10))

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer_and_plot(wafer3737, logcurve_4BARO, min_logcurve_4BARO, c(-10,-20,5,0))

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer_and_plot(wafer3737, logcurve_5BARO, min_logcurve_5BARO, c(-10,-30,5,0,5))

## ----fig.width = 8, fig.height = 8---------------------------------------
area_cost_fun = function(datax,datay,fity){
  sum((0.5*diff(datax)*(datay[1:length(datax)-1] + datay[2:length(datax)] 
                        - fity[1:length(datax)-1] - fity[2:length(datax)]))^2)
}
min_logcurve_5BAROA = function(param, datax, datay){
  z = logcurve_5BARO(datax, param)
  area_cost_fun(datax,datay,z)
}
fit_wafer_and_plot(wafer3737, logcurve_5BARO, min_logcurve_5BAROA, c(-10,-30,5,0,5))

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer_and_plot(wafer4464, logcurve_5BARO, min_logcurve_5BAROA, c(-10,-30,5,0,5))


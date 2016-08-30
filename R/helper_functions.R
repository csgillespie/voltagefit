# Helper functions

## logcurve
## Function for the forwards curve
logcurve = function(x, param){
  #param[1] + param[2] / (1 + exp(param[3] + param[4]*x + param[5]*x^2 + param[6]*x^3))
  cfbar = (2*param[3]*param[5])/abs(param[3]+param[5])
  fx = 1/(1+exp(-cfbar*(x-param[4])))
  param[1] + param[2] / (1 + fx*exp(param[3]*(x-param[4])) + (1-fx)*exp(param[5]*(x-param[4])))
}

## min_logcurve
## The function to be minimised by nlm to fit the forward logcurve,
## subject to the cost function (cost_fun)
min_logcurve = function(param, datax, datay){
  #z = logcurve(datax, param)
  #res = cost_fun(z, datay) 
  interp = approx(datax,datay,n = length(datax))
  z = logcurve(interp$x, param)
  res = cost_fun(z, interp$y) 
  
  sum(res[!is.nan(res)])
}

## logcurve_back
## The function for the backwards curve.
## This is in the same form as logcurve but requires a parameter
## structure of forward+backwards, the thinking being, that if the
## forward and backward curves are perfect for the same device then
## the backwards parameters would all be zero.
## i.e. the forward and backward curves characterised by the same parameters
# logcurve_back = function(x, paramf, paramb){
#   logcurve(x, paramf + paramb)
#   #paramf[1]+paramb[1]+(paramf[2]+paramb[2]-(paramf[1]+paramb[1]))/(1+exp(paramf[3]+paramb[3]+(paramf[4]+paramb[4])*x+(paramf[5]+paramb[5])*x^2+(paramf[6]+paramb[6])*x^3))
# }
# 
# ## min_logcurve_back is the function to be minimised by nlm to fit backwards
# ## logcurve, subject to the cost function (cost_fun)
# min_logcurve_back = function(x, datax, datay, paramf){
#   #x is what will be used (changed) to minimise, i.e param values 
#   z = logcurve_back(datax, paramf, x)
#   res = cost_fun(z, datay) 
#   sum(res[!is.nan(res)])
# }

## cost_fun is the cost function, minimised by nlm
cost_fun = function(x,y){
  (x-y)^2
  #1/log((x-y)^2)
}

## residual creates the residuals (currently not used!)
#residual = function(pred, x){
#  x-pred
#}

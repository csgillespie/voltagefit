# helper functions

## logcurve is the function for the forwards curve
logcurve = function(x, param){
  param[1] + (param[2] - param[1]) / (1 + exp(param[3] + param[4]*x + param[5]*x^2 + param[6]*x^3))
}

## minlogcurve is the function to be minimised by nlm to fit forward logcurve
minlogcurve = function(x, datax, datay){
  #x is what will be used (changed) to minimise, i.e param values 
  z = logcurve(datax, x)
  res = costfun(z,datay) #(z - datay)^2
  sum(res[!is.nan(res)])
}

## logcurveb is the function for the backwards curve
logcurveb = function(x, paramf, paramb)
{
  logcurve(x, paramf + paramb)
  #paramf[1]+paramb[1]+(paramf[2]+paramb[2]-(paramf[1]+paramb[1]))/(1+exp(paramf[3]+paramb[3]+(paramf[4]+paramb[4])*x+(paramf[5]+paramb[5])*x^2+(paramf[6]+paramb[6])*x^3))
}

## minlogcurveb is the function to be minimised by nlm to fit forward logcurve
minlogcurveb = function(x, datax, datay, paramf){
  #x is what will be used (changed) to minimise, i.e param values 
  z = logcurveb(datax, paramf, x)
  res = costfun(z,datay) #(z - datay)^2
  sum(res[!is.nan(res)])
}

costfun = function(x,y){
  (x-y)^2
}

## residual creates the residuals 
#residual = function(pred, x){
#  x-pred
#}

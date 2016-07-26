## function to get parameters to create curves for each week
## fm is of the form, fitmanova
##
## the function returns a data frame looking like
##   direction week         X1         X2          X3         X4          X5          X6
## 1   Forward    1 0.99225085 0.34297343 -0.95642961  1.7080048 -0.60900038  0.10555078
## 2   Forward    2 0.98700323 0.32394634 -0.08862228  1.0466092 -0.30680664  0.05753791
## 3  Backward    1 0.03496712 0.02715283 -0.49391182 -0.3998133  0.35780175 -0.07398364
## 4  Backward    2 0.01187002 0.02869879 -0.52432629  0.1861148  0.02938187 -0.02054951
##
## direction = whether it is the forward or backward curve
## week = the week the curve corresponds to
## X1 - X6 = the parameters characterising the (week) curves

weekparam = function(fm){
  param_f = matrix(0, nrow = length(fm$forward$man_w$xlevels$`factor(week)`), 
                   ncol = ncol(fm$forward$man_w$coefficients)) 
  param_b = param_f  
  
  for (i in 1:6){
    for (j in 1:(nrow(param_f) - 1)){
      ## for each parameter, extract and combine from MANOVA output. Both forward and backward 
      param_f[j,i] = fm$forward$man_w$coefficients[1,i] + fm$forward$man_w$coefficients[j+1,i]
      param_b[j,i] = fm$backward$man_w$coefficients[1,i] + fm$backward$man_w$coefficients[j+1,i]
    }
    ## calculate the last parameter from the sum of all the others
    param_f[nrow(param_f),i] = fm$forward$man_w$coefficients[1,i] - sum(fm$forward$man_w$coefficients[2:nrow(param_f),i])
    param_b[nrow(param_f),i] = fm$backward$man_w$coefficients[1,i] - sum(fm$backward$man_w$coefficients[2:nrow(param_f),i])
  }
  
  forward = data.frame(direction="Forward", week = fm$forward$man_w$xlevels$`factor(week)`, param_f)
  backward = data.frame(direction="Backward", week = fm$forward$man_w$xlevels$`factor(week)`, param_b)
  
  return(rbind(forward,backward))
}

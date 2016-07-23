## function to get parameters to create curves for each week
## fm is of the form, fitmanova

weekparam = function(fm){
  param_f = matrix(0, nrow = length(fm$forward$man_w$xlevels$`factor(week)`), 
                   ncol = ncol(fm$forward$man_w$coefficients)) 
  param_b = param_f  
  
  for (i in 1:6){
    for (j in 1:(nrow(param_f) - 1)){
      param_f[j,i] = fm$forward$man_w$coefficients[1,i] + fm$forward$man_w$coefficients[j+1,i]
      param_b[j,i] = fm$backward$man_w$coefficients[1,i] + fm$backward$man_w$coefficients[j+1,i]
    }
    param_f[nrow(param_f),i] = fm$forward$man_w$coefficients[1,i] - sum(fm$forward$man_w$coefficients[2:nrow(param_f),i])
    param_b[nrow(param_f),i] = fm$backward$man_w$coefficients[1,i] - sum(fm$backward$man_w$coefficients[2:nrow(param_f),i])
  }
  
  forward = data.frame(direction="Forward", week = fm$forward$man_w$xlevels$`factor(week)`, param_f)
  backward = data.frame(direction="Backward", week = fm$forward$man_w$xlevels$`factor(week)`, param_b)
  
  return(rbind(forward,backward))
}

## function to get parameters to create curves for each week
## fm is of the form, fitmanova

weekparam = function(fm)
{
  #options(contrasts=c("contr.sum","contr.sum"))
  #man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$forward$parameters,weights=1/theta$forward$cost) 
  
  param_f = matrix(0,nrow=length(fm$weight$forward$xlevels$`factor(week)`),ncol=ncol(fm$weight$forward$coefficients)) 
  param_b = param_f  
  
  for(i in 1:6)
  {
    for(j in 1:(nrow(param_f)-1))
    {
      param_f[j,i] = fm$weight$forward$coefficients[1,i] + fm$weight$forward$coefficients[j+1,i]
      param_b[j,i] = fm$weight$backward$coefficients[1,i] + fm$weight$backward$coefficients[j+1,i]
    }
    param_f[nrow(param_f),i] = fm$weight$forward$coefficients[1,i] - sum(fm$weight$forward$coefficients[2:nrow(param_f),i])
    param_b[nrow(param_f),i] = fm$weight$backward$coefficients[1,i] - sum(fm$weight$backward$coefficients[2:nrow(param_f),i])
  }
  
  return(list(forward=param_f,backward=param_b))
}

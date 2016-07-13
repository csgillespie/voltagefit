## function to get parameters to create curves for each week
## theta is of the form, fitall

weekparam = function(theta)
{
  options(contrasts=c("contr.sum","contr.sum"))
  man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$forward$parameters,weights=1/theta$forward$cost) 
  
  param_f = matrix(0,nrow=length(unique(theta$forward$parameters$w)),ncol=6)  

  for(i in 1:6)
  {
    for(j in 1:(nrow(param_f)-1))
    {
      param_f[j,i] = man$coefficients[1,i] + man$coefficients[j+1,i]
    }
    param_f[nrow(param_f),i] = man$coefficients[1,i] - sum(man$coefficients[2:nrow(param_f),i])
  }
  
  man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$backward$parameters,weights=1/theta$forward$cost) 
  
  param_b = matrix(0,nrow=nrow(param_f),ncol=6)  
  
  for(i in 1:6)
  {
    for(j in 1:(nrow(param_f)-1))
    {
      param_b[j,i] = man$coefficients[1,i] + man$coefficients[j+1,i]
    }
    param_b[nrow(param_f),i] = man$coefficients[1,i] - sum(man$coefficients[2:nrow(param_f),i])
  }
  
  return(list(forward=param_f,backward=param_b))
}

## function to get parameters to create curves for each week
## fited is of the form, fitwafer, tot_obs is the total number of 
## observations made on each device within a wafer (default=241)

weekparam = function(wafer,fitted,tot_obs=241)
{
  theta = thetasetup(wafer,fitted,tot_obs)
  
  options(contrasts=c("contr.sum","contr.sum"))
  man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$forward,weights=1/fitted$forward$cost) 
  
  param_f = matrix(0,nrow=length(unique(wafer$test_date)),ncol=6)  
  
  for(i in 1:6)
  {
    for(j in 1:(length(unique(wafer$test_date))-1))
    {
      param_f[j,i] = man$coefficients[1,i] + man$coefficients[j+1,i]
    }
    param_f[length(unique(wafer$test_date)),i] = man$coefficients[1,i] - sum(man$coefficients[2:length(unique(wafer$test_date)),i])
  }
  
  man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$backward,weights=1/fitted$forward$cost) 
  
  param_b = matrix(0,nrow=length(unique(wafer$test_date)),ncol=6)  
  
  for(i in 1:6)
  {
    for(j in 1:(length(unique(wafer$test_date))-1))
    {
      param_b[j,i] = man$coefficients[1,i] + man$coefficients[j+1,i]
    }
		param_b[length(unique(wafer$test_date)),i] = man$coefficients[1,i] - sum(man$coefficients[2:length(unique(wafer$test_date)),i])
  }
  
  return(list(forward=param_f,backward=param_b))
}

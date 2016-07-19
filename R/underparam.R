## function to create underlying parameters
## fm is of the form, fitmanova

underparam = function(fm)
{
  t_all_f = fm$weight$forward$coefficients[1,] 
  vmat_f = vcov(fm$noweight$forward)
  
  t_all_b = fm$weight$backward$coefficients[1,]
  vmat_b = vcov(fm$noweight$backward)
  
  loc=numeric(6)
  for(i in 1:6)
  {
    loc[i] = 1+(i-1)*(length(fm$weight$forward$xlevels$`factor(week)`)+length(fm$weight$forward$xlevels$`factor(name)`))-(i-1) 
  }
  
  t_vmat_f = matrix(0,ncol=6,nrow=6)
  t_vmat_b = t_vmat_f
  
  for(i in 1:6)
  {
    t_vmat_f[i,] = vmat_f[loc[i],loc]
    t_vmat_b[i,] = vmat_b[loc[i],loc]
  }  

  return(list(forward=list(param = t_all_f,var = t_vmat_f),backward=list(param = t_all_b,var = t_vmat_b)))
}

## function to create sample used for underlying curves
##
## underparam is of the form, underparam
## n is the number of samples required

undercurvesim = function(underparam,n=1000)
{
  t_f_sim=mvrnorm(n,underparam$forward$param,underparam$forward$var)
  t_b_sim=mvrnorm(n,underparam$backward$param,underparam$backward$var)

  return(list(forward=t_f_sim,backward=t_b_sim))
}

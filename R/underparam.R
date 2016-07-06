## function to create underlying parameters
## fited is of the form, fitwafer, tot_obs is the total number of 
## observations made on each device within a wafer (default=241), 

underparam = function(wafer,fitted,tot_obs=241)
{
  theta = thetasetup(wafer,fitted,tot_obs)
  
  options(contrasts=c("contr.sum","contr.sum"))
  man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$forward,weights=1/fitted$forward$cost) 
  
  t_all=c(man$coefficients[1,1],
          man$coefficients[1,2],
          man$coefficients[1,3],
          man$coefficients[1,4],
          man$coefficients[1,5],
          man$coefficients[1,6])
  
  m=lm(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$forward)
  vmat=vcov(m)
  
  loc=numeric(6)
  for(i in 1:6)
  {
    loc[i] = 1+(i-1)*(length(unique(wafer$test_date))+length(unique(wafer$name)))-(i-1) 
  }

  t_vmat=matrix(0,ncol=6,nrow=6)
  for(i in 1:6)
  {
    t_vmat[i,]=vmat[loc[i],loc]
  }  
  forward = list(param = t_all,var = t_vmat)
  
  man = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$backward,weights=1/fitted$forward$cost) 
  
  t_all=c(man$coefficients[1,1],
          man$coefficients[1,2],
          man$coefficients[1,3],
          man$coefficients[1,4],
          man$coefficients[1,5],
          man$coefficients[1,6])
  
  m=lm(cbind(X1,X2,X3,X4,X5,X6) ~ factor(w)+factor(n), data=theta$backward)
  vmat=vcov(m)
  
  t_vmat=matrix(0,ncol=6,nrow=6)
  for(i in 1:6)
  {
    t_vmat[i,]=vmat[loc[i],loc]
  }  
  backward = list(param = t_all,var = t_vmat)
  
  return(list(forward=forward,backward=backward))
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

## function to create underlying parameters
## fm is of the form, fitmanova

underparam = function(fm)
{
  t_all_f = fm$forward$man_w$coefficients[1,] 
  t_all_b = fm$backward$man_w$coefficients[1,]

  loc = 1 + (0:5)*(length(fm$forward$man_w$xlevels$`factor(week)`))  

  t_vmat_f = matrix(0, ncol = 6, nrow = 6)
  t_vmat_b = t_vmat_f
  
  for (i in 1:6){
    t_vmat_f[i,] = fm$forward$varcov[loc[i], loc]
    t_vmat_b[i,] = fm$backward$varcov[loc[i], loc]
  }  
  
  forward = data.frame(direction="Forward", t(t_all_f))
  backward = data.frame(direction="Backward", t(t_all_b))
  
  return(list(param = rbind(forward, backward), var = list(forward = t_vmat_f, backward = t_vmat_b)))
}

## function to create sample used for underlying curves
##
## underparam is of the form, underparam
## n is the number of samples required

undercurvesim = function(underparam, n=1000)
{
  t_f_sim = mvrnorm(n, as.numeric(underparam$param[underparam$param$direction=="Forward",][,2:7]), underparam$var$forward)
  t_b_sim = mvrnorm(n, as.numeric(underparam$param[underparam$param$direction=="Backward",][,2:7]), underparam$var$backward)
  
  forward = data.frame(direction="Forward", t_f_sim)
  backward = data.frame(direction="Backward", t_b_sim)
  
  return(rbind(forward, backward))
}

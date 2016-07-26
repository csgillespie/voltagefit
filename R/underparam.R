## function to get parameters to create underlying curve
## fm is of the form, fitmanova
##
## the function returns a list with elements param and var
## param is a data frame
##  direction         X1         X2         X3         X4         X5          X6
## 1   Forward 0.98962704 0.33345989 -0.5225259  1.3773070 -0.4579035  0.08154434
## 2  Backward 0.02341857 0.02792581 -0.5091191 -0.1068493  0.1935918 -0.04726657
##
## direction = whether it is the forward or backward curve
## X1 - X6 = the parameters characterising the (week) curves
##
## var is a list with elements forward and backward, with each 
## containing the covariance matrix (for use in undercurvesim)

underparam = function(fm)
{
  t_all_f = fm$forward$man_w$coefficients[1,]  ## get underlying parameters form MANOVA
  t_all_b = fm$backward$man_w$coefficients[1,]

  loc = 1 + (0:5)*(length(fm$forward$man_w$xlevels$`factor(week)`))  ## index which elements in vcov we require

  t_vmat_f = matrix(0, ncol = 6, nrow = 6)
  t_vmat_b = t_vmat_f
  
  for (i in 1:6){
    t_vmat_f[i,] = fm$forward$varcov[loc[i], loc]  ## extract the elements of vcov we need
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
## 
## the function returns a data frame
##   direction        X1        X2         X3       X4         X5         X6
## 1   Forward 0.9830902 0.3333241 -0.5907498 1.474182 -0.5081067 0.09033372
## 2   Forward 0.9935583 0.3314643 -0.2409134 1.113279 -0.3506499 0.06711141
## 3   Forward 0.9879220 0.3325370 -0.3994582 1.265018 -0.4056076 0.07313676
##
## direction = whether it is the forward or backward curve
## X1 - X6 = the parameters characterising the curves

undercurvesim = function(underparam, n=1000)
{
  ## generate sample from multivariate Normal
  t_f_sim = mvrnorm(n, as.numeric(underparam$param[underparam$param$direction=="Forward",][,2:7]), underparam$var$forward)
  t_b_sim = mvrnorm(n, as.numeric(underparam$param[underparam$param$direction=="Backward",][,2:7]), underparam$var$backward)
  
  forward = data.frame(direction="Forward", t_f_sim)
  backward = data.frame(direction="Backward", t_b_sim)
  
  return(rbind(forward, backward))
}

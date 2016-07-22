## function to fit manova to the results of fitall
##
## fitall is of the form fitall, design is the design matrix with columns 
## week, wafer, replicate, treatment (ordered as such)

fitmanova = function(fitall, design){
  options(contrasts = c("contr.sum", "contr.sum"))
  
  fdata = cbind(week = design[,1], fitall$forward$parameters)
  bdata = cbind(week = design[,1], fitall$backward$parameters)
  
  man_w_f = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week) + factor(id), 
                   data = fdata, weights = 1 / fitall$forward$cost$cost) 
  m_f = lm(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week) + factor(id), data = fdata)
  
  man_w_b = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week) + factor(id), 
                   data = bdata, weights = 1 / fitall$forward$cost$cost) 
  m_b = lm(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week) + factor(id), data = bdata)
  
  weight = list(forward = man_w_f, backward = man_w_b)
  noweight = list(forward = m_f, backward = m_b)
  
  return(list(weight = weight, noweight = noweight))
}

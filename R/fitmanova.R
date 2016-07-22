## function to fit manova to the results of fitall
##
## fitall is of the form fitall, design is the design matrix with columns 
## week, wafer, replicate, treatment (ordered as such)

fitmanova = function(fitall, design){
  op = getOption("contrasts")
  options(contrasts = c("contr.sum", "contr.sum"))
  on.exit(options(contrasts = op))
  
  fdata = cbind(week = design$week, treatment = design$treatment, fitall$forward$parameters)
  bdata = cbind(week = design$week, treatment = design$treatment, fitall$backward$parameters)
  
  man_w_f = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week), 
                   data = fdata, weights = 1 / fitall$forward$cost$cost) 
  m_f = lm(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week), data = fdata)
  
  man_w_b = manova(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week), 
                   data = bdata, weights = 1 / fitall$forward$cost$cost) 
  m_b = lm(cbind(X1,X2,X3,X4,X5,X6) ~ factor(week), data = bdata)
  
  weight = list(forward = man_w_f, backward = man_w_b)
  noweight = list(forward = m_f, backward = m_b)
  
  return(list(weight = weight, noweight = noweight))
}

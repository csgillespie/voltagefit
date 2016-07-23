man = function(data, weight){
  man_w = manova(data.matrix(data[,8:13]) ~ factor(week), 
                   data = data, weights = 1 / weight) 
  varcov = vcov(lm(data.matrix(data[,8:13]) ~ factor(week), data = data))
  
  return(list(man_w = man_w, varcov = varcov))
}


## function to fit manova to the results of fitall
##
## fitall is of the form fitall, design is the design matrix with columns 
## week, wafer, replicate, treatment (ordered as such)

fitmanova = function(fitall, design){
  op = getOption("contrasts")
  options(contrasts = c("contr.sum", "contr.sum"))
  on.exit(options(contrasts = op))
  
  fdata = cbind(week = design$week, treatment = design$treatment, f[f$direction=="Forward",])
  bdata = cbind(week = design$week, treatment = design$treatment, f[f$direction=="Backward",])
  
  # HOW TO GET TREATMENT IN TO BELOW WITHOUT IT BREAKING!!!!
  
  f = man(fdata, fdata$cost)
  b = man(bdata, fdata$cost)
  
  return(list(forward = f, backward = b))
}

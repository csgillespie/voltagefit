## function to perform the MANOVA parts of the analysis
## data is the data frame of parameter values, appended with week 
## and treatment type for each device 

## TREATMENT NEEDS TO BE INCORPORATED!!! HOW?????

## weight is the weights to be used in the MANOVA, (applied as 1/weight) 
## the function returns a list, a MANOVA class item (incorporating weights)
## and the output of vcov (without weights)
##
## N.B. vcov does not work on output which incorporates weights,
## the use of vcov helps with correlation structure when simulating 
## values for the underlying curve (otherwise odd curves were evident 
## from bizarre parameter combinations)

man = function(data, weight){
  man_w = manova(data.matrix(data[,8:13]) ~ factor(week), 
                   data = data, weights = 1 / weight) 
  varcov = vcov(lm(data.matrix(data[,8:13]) ~ factor(week), data = data))
  
  return(list(man_w = man_w, varcov = varcov))
}


## function to fit MANOVA (using man function) to the results of fitall
##
## fitall is of the form fitall, design is the design matrix (of the form 
## data.frame) with columns week, wafer, replicate, treatment
##
## the function returns a list with elements forward and backward, 
## each containing the MANOVA output incorporating weights and 
## vcov (excluding weights)

fitmanova = function(fitall, design){
  op = getOption("contrasts")
  options(contrasts = c("contr.sum", "contr.sum"))
  on.exit(options(contrasts = op))
  
  len = tapply(fitall[fitall$direction=="Forward",]$id,fitall[fitall$direction=="Forward",]$id, length)
  week = rep(design[design$wafer==names(len),]$week, len)
  treatment = rep(design[design$wafer==names(len),]$treatment, len)
  
  fdata = data.frame(week = week, treatment = treatment, fitall[fitall$direction=="Forward",])
  bdata = data.frame(week = week, treatment = treatment, fitall[fitall$direction=="Backward",])
  
  # HOW TO GET TREATMENT IN TO BELOW WITHOUT IT BREAKING!!!!
  
  f = man(fdata, fdata$cost)
  b = man(bdata, fdata$cost)
  
  return(list(forward = f, backward = b))
}

## function to demo the functions included with this package,
## we fit to two weeks of data

waferdemo = function(){
  # fit all files within a directory, returning the paarmeters  
  # associated with each curve on a given wafer
  fit = fitall("test/")
  
  # look at the parameters and cost functions
  histplot(fit)
  
  #make the design matrix, dmat
  week = c(1,1,1,1,2,2)
  wafer = unique(f$id)
  replicate = c(1:6)
  treatment = rep(1,6)
  design = data.frame(week = week, wafer = wafer, replicate = replicate, treatment = treatment)
  
  # perform MANOVA on the output of fitall
  man = fitmanova(fit, design)
  
  # calculate the parameters associated with a curve for a given week
  weekp = weekparam(man)
  
  # calculate the parameters associated with the underlying curve
  underp = underparam(man)
  
  #generate a sample from the underlying curve parameter distribution 
  unders = undercurvesim(underp, 1000)
  
  # calculate the curves for a given week
  weekc = weekcurve(fit, weekp)
  
  # calculate the underlying curve
  underc = undercurve(fit, unders)
  
  readline("Press enter for the next plot") 
  #plot the week curves
  plotweek(fit, weekc, man, T)
  
  readline("Press enter for the next plot") 
  #plot the underlying curve
  plotunder(fit, underc, T, T)
}

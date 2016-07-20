## function to demo the functions included with this package,
## we fit to two weeks of data
##

waferdemo = function()
{
  # fit all files within a directory, returning the paarmeters  
  # associated with each curve on a given wafer
  fit = fitall("data/")
  
  # look at the parameters and cost functions
  histplot(fit)
  
  #make the design matrix, dmat
  week = rep(1:2,c(46,26))
  wafer = rep(c(1:6),c(13,8,13,12,13,13))
  replicate=c(rep(1:4,c(13,8,13,12)),rep(1:2,c(13,13)))
  treatment=rep(1,72)
  dmat=matrix(c(week,wafer,replicate,treatment),ncol=4,byrow=F)
  
  # perform MANOVA on the output of fitall
  man = fitmanova(fit,dmat)
  
  # calculate the parameters associated with a curve for a given week
  weekp = weekparam(man)
  
  # calculate the parameters associated with the underlying curve
  underp = underparam(man)
  
  #generate a sample from the underlying curve parameter distribution 
  unders = undercurvesim(underp,1000)
  
  # import the volatge gates which we will plot curves against 
  temp = readRDS("data/3737.rds")
  v = temp$VG[1:241]
  
  # calculate the curves for a given week
  weekc = weekcurve(weekp,v)
  
  # calculate the underlying curve
  underc = undercurve(unders,v)
  
  readline("Press enter for the next plot") 
  #plot the week curves
  plotweek(fit,weekc,man,T,v)
  
  readline("Press enter for the next plot") 
  #plot the underlying curve
  plotunder(fit,underc,T,T,v)
}

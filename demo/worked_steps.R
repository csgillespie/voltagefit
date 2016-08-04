## THIS FILE SHOULD BE USED LOCALLY TO DEMONSTRATE THE STEPS IN AN ANALYSIS
## Initially source the R files needed 

## Fit the model to a single wafer
## Here we fit to wafer3737
fit = fitwafer(wafer3737)

## Fit the model to all files in a directory
## Here we fit to two weeks of data
## Week 1: wafers = 3737,3757,3758,3759 (4 wafers)
## Week 2: wafer = 4464,4465 (2 wafers)
fit = fitall("inst/extdata/")

## If you wanted at this point you could run 
## curves(f,f)
## and then plot the output of each curve (device) on the wafers
## N.B This would be the option if we only considered a single week

## look at the parameter values and cost function
histplot(fit)

## make the design matrix, bespoke for each setup 
week = c(1,1,1,1,2,2)
wafer = unique(fit$id)
replicate = c(1:6)
treatment = rep(1,6)
design = data.frame(week = week, wafer = wafer, replicate = replicate, treatment = treatment)
##   week wafer replicate treatment
## 1    1  3737         1         1
## 2    1  3757         2         1
## 3    1  3758         3         1
## 4    1  3759         4         1
## 5    2  4464         5         1
## 6    2  4465         6         1


## Fit MANOVA to the output of fit 
## (performing MANOVA with the parameter values as the response)
fitman = fitmanova(fit, design)


## look at the output of the MANOVA, using commands such as 
summary(fitman$forward$man_w)
summary(fitman$backward$man_w)


## Calculate the week parameters (the parameters for each curve 
## corresponding to each week)
weekp = weekparam(fitman)


## Calculate the underlying parameters (the parameters for the 
## underlying curve)
underp = underparam(fitman)


## Generate a sample of parameters for the underlying curve
unders = undercurvesim(underp)


## Calculate the week curves
weekc = curves(fit, weekp)


## Calculate the underlying curves (one from each in the sample)
underc = curves(fit, unders)


## Plot the week curves
## On transformed scale (on which model fit takes place)
plotweek(fit, weekc, fitman, F)

# On original scale
plotweek(fit, weekc, fitman, T)


## Plot the underlying curve
## On transformed scale (on which model fit takes place)
## using the mean curve
plotunder(fit, underc, F, F)

## On transformed scale (on which model fit takes place)
## using the median curve
plotunder(fit, underc, F, T)

# On original scale
## using the mean curve
plotunder(fit, underc, T, F)

# On original scale
## using the median curve
plotunder(fit, underc, T, T)

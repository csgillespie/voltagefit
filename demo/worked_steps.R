## Installing the package
#The package lives on [github](https://github.com/csgillespie/voltagefit/) and can be installed using
#library("voltagefit")

## Example data
#The package contains some example data sets that can be accessed via
data_dir = system.file("extdata/", package="voltagefit")
list.files(data_dir)

## A single wafer
#We can load a single wafer
## This is just a data frame. 
## I expect the data will come from a database
wafer_3737 = readRDS(file.path(data_dir, "3737.rds"))

#To fit the model to a single wafer, we use `fit_wafer`
fit_3737 = fit_wafer(wafer_3737, verbose=FALSE)
fit_3737
#The object `fit_3737` is a data frame.
#Each row corresponds to a particular curve on the wafer. The columns are...
# `id`: a unique idenfier for the wafer;
# `cost`: the error term from the curve fitting process;
# `direction`: `Forward` or `Backward`
# `X1`, ..., `X6`: Parameters relating to the logistic model.

## Multiple wafers
#First load in example data.
## In real life, pull data from a database
all_wafers = fit_all(data_dir, verbose=FALSE)
#You can also view the parameters
plot(all_wafers, cost=FALSE)
hist(all_wafers)

## Design matrix
#For each voltage curve, we have fitted a logistic model, obtained parameter estimates
#and estimated how well the curve fits. This next stage attempts to estimate
#week and treatment effects. Using the data in `all_wafers`, we first create a design
#matrix descriping the experimental set-up. The design matrix should have
#three columns
wafer = unique(all_wafers$id)
week = c(1, 1, 1, 1, 2, 2)
treatment = rep(1, 6)
(design = data.frame(wafer = wafer, week = week, treatment = treatment))

#We then fit a MANOVA model, using the parameter values as the response
fit_man = fit_manova(all_wafers, design)
params = get_params(fit_man)

#MANOVA is just a multivariate version of the ANOVA table. As such, we can
#obtain a $p$-value for week differences using
#fit_man$forward$man_w
#summary(fit_man$backward$man_w)
  
#We can now get the mean parameters
means = mean(fit_man)
#and plot them
library(ggplot2)
ggplot(means) + geom_line(aes(x, y, colour=type)) + scale_y_log10() + facet_grid(~direction)

## Generate a sample of parameters for the underlying curve
unders = sample(fit_man,  n=20)
#head(unders)
#unders = dd
ggplot(unders$samples) + 
  geom_line(aes(x, y, group=sim_no, colour=type)) + scale_y_log10() + 
  facet_grid(~direction)


## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("csgillespie/voltagefit")

## ---- message=FALSE------------------------------------------------------
library("voltagefit")

## ----eval=FALSE----------------------------------------------------------
#  library("doParallel")
#  registerDoParallel()

## ------------------------------------------------------------------------
data_dir = system.file("extdata/", package="voltagefit")
list.files(data_dir)

## ------------------------------------------------------------------------
## This is just a data frame. 
## I expect the data will come from a database
wafer_3737 = readRDS(file.path(data_dir, "3737.rds"))

## ------------------------------------------------------------------------
fit_3737 = fit_wafer(wafer_3737, verbose=FALSE)
fit_3737

## ------------------------------------------------------------------------
data_dir = system.file("extdata/", package="voltagefit")
## In real life, pull data from a database
all_wafers = fit_all(data_dir, verbose=FALSE)

## ----fig.width = 8-------------------------------------------------------
plot(all_wafers)
hist(all_wafers)

## ------------------------------------------------------------------------
wafer = unique(all_wafers$id)
week = c(1, 1, 1, 1, 2, 2)
treatment = rep(1, 6)
(design = data.frame(wafer = wafer, week = week, treatment = treatment))

## ------------------------------------------------------------------------
fit_man = fit_manova(all_wafers, design)
(params = get_params(fit_man))

## ------------------------------------------------------------------------
means = mean(fit_man)

## ----fig.width = 8-------------------------------------------------------
library(ggplot2)
ggplot(means) + geom_line(aes(x, y, colour=type)) + scale_y_log10() + 
  facet_grid(~direction)

## ----fig.width = 8-------------------------------------------------------
## Generate a sample of parameters for the underlying curve
unders = sample(fit_man,  n=20)
#head(unders)

#unders = dd
ggplot(unders$samples) + 
  geom_line(aes(x, y, group=sim_no, colour=type)) + scale_y_log10() + 
  facet_grid(~direction)



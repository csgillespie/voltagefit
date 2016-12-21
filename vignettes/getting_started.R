## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_bw())

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("csgillespie/voltagefit")

## ---- message=FALSE------------------------------------------------------
library("voltagefit")

## ----eval=FALSE----------------------------------------------------------
#  library("doParallel")
#  ## This sets the number of cores to use.
#  no_of_cores = max(parallel::detectCores() - 1, 1)
#  registerDoParallel(no_of_cores)

## ------------------------------------------------------------------------
e = new.env()
data("voltage", package="voltagefit", envir = e)
ls(envir = e)

## ------------------------------------------------------------------------
head(get("wafer5210", envir = e), 3)

## ---- echo=1, eval=FALSE-------------------------------------------------
#  wafer_5210 = fit_wafer(get("wafer5210", envir=e))
#  saveRDS(wafer_5210, file="wafer_5210.Rds")

## ----echo=FALSE----------------------------------------------------------
wafer_5210 = readRDS(file="wafer_5210.Rds")

## ---- message=FALSE, eval=FALSE, echo=1:2--------------------------------
#  l = lapply(ls(envir = e),
#             function(i) fit_wafer(get(i, envir = e)))
#  all_wafers = Reduce(rbind, l)
#  saveRDS(all_wafers, file="all_wafers.Rds")

## ----echo=FALSE----------------------------------------------------------
all_wafers = readRDS(file="all_wafers.Rds")

## ----fig.width = 8-------------------------------------------------------
plot(all_wafers)
hist(all_wafers)

## ------------------------------------------------------------------------
wafer = unique(all_wafers$id)
week = c(rep(1:2, each=8), 1)
treatment = c(rep(1, 9), rep(2,8))
(design = data.frame(wafer = wafer, week = week, treatment = treatment))

## ------------------------------------------------------------------------
fit_man = fit_manova(all_wafers, design)
(params = get_params(fit_man))

## ------------------------------------------------------------------------
summary(fit_man$forward$man_w)

## ------------------------------------------------------------------------
means = mean(fit_man)

## ----fig.width = 8-------------------------------------------------------
library("ggplot2")
ggplot(means) + 
  geom_line(aes(VG, ID, colour=type)) + scale_y_log10() + 
  facet_grid(~direction)

## ----fig.width = 8-------------------------------------------------------
## Generate a sample of parameters for the underlying curve
unders = sample(fit_man, n = 20)
ss = unders$samples
ggplot(ss) + 
  geom_line(aes(VG, ID, group=sim), alpha=0.1) + scale_y_log10() + 
  facet_grid(type~direction) +
  geom_line(data=means, aes(VG, ID), colour="steelblue")

## ------------------------------------------------------------------------
#treatment2 - baseline
curve_d = curve_diff_mean(fit_man, type=c("baseline", "treatment2"), direction="Forward")
ggplot(curve_d, aes(VG, diff)) + 
  geom_line()

## ------------------------------------------------------------------------
#treatment2 - baseline
curve_d = curve_diff_sample(fit_man, type=c("baseline", "treatment2"), n = 100,  
                          direction="Forward")
ggplot(curve_d, aes(VG, diff)) + 
  geom_line(aes(group=sim), alpha=0.05) + 
  stat_smooth()


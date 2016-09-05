## ---- echo=FALSE, results="hide"-----------------------------------------
library(voltagefit)

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer(wafer3737,cost_func=sum_residuals, dev_curve=curve_exp_poly, 
          initparams = c(-9.1123, -15.5432, 1.1966,0.6834, -0.1843, 0.1418),
          plot=TRUE)

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer(wafer3737,cost_func=sum_residuals, dev_curve=curve_tanh, 
          initparams = c(-10,10,0.5,0),plot=TRUE)

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer(wafer3737,cost_func=sum_residuals, dev_curve=curve_4PL, 
          initparams = c(-30,5,100,-10),plot=TRUE)

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer(wafer3737,cost_func=sum_residuals, dev_curve=curve_4BARO, 
          initparams = c(-10,-30,5,0),plot=TRUE)

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer(wafer3737,cost_func=sum_residuals, dev_curve=curve_5BARO, 
          initparams = c(-10,-30,5,0,5),plot=TRUE)

## ----fig.width = 8, fig.height = 8---------------------------------------
fit_wafer(wafer3737,cost_func=area_between_curves, dev_curve=curve_5BARO, 
          initparams = c(-10,-30,5,0,5),plot=TRUE)


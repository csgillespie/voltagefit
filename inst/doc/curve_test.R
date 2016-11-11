## ---- message=FALSE------------------------------------------------------
library("voltagefit")
library("doParallel")
registerDoParallel(7)
data("voltagefitWafers")
wafers = grep("wafer[0-9]+",ls(), value=TRUE)

## ---- message=FALSE,fig.width = 4,fig.show='hold'------------------------
for (w in wafers){
  name = paste("wafer",get(w)$wafer_id[1],sep="")
  message(name)
  fit = fit_wafer(get(w))
  print(plot_curves(fit,dir="Forward"))
  print(plot_curves(fit,dir="Backward"))
}


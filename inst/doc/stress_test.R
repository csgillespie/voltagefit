## ----message=F-----------------------------------------------------------
library(ggplot2)
library(voltagefit)

## ------------------------------------------------------------------------
tmpdata = add_forward_backward(wafer3737)
sample_forward_curve = tmpdata[tmpdata$name=="25.10.03.B" & tmpdata$direction=="Forward",]
datax = sample_forward_curve$VG

## ----fig.width = 7,fig.height = 4----------------------------------------

param = c(-10,-20,1,1)
simy = curve_4BARO(datax, param);
sim_curve = data.frame(datax=datax,simy=exp(simy))
ggplot(data=sim_curve, aes(x=datax, y=simy)) + geom_line() + scale_y_log10() +
  ylab("ID") + xlab("VG")

## ----fig.width = 7,fig.height = 4----------------------------------------
sim_curves = NULL
t1 = rnorm(100,-10,1)
t2 = rnorm(100,-20,1)
t3 = rnorm(100,1,0.1)
t4 = rnorm(100,1,0.1)
for (i in 1:100) {
  simy = curve_4BARO(datax, c(t1[i],t2[i],t3[i],t4[i]));
  sim_curves = rbind(sim_curves,data.frame(VG=datax,ID=exp(simy),name=as.character(i),
                wafer_id=1,id=1,ERF=1,row=1,col=1,comments="",test_num=1,test_date="1",
                result=-1))
}
ggplot(data=sim_curves, aes(x=VG, y=ID, group=name,color=name)) + geom_line() + scale_y_log10() +
  ylab("ID") + xlab("VG") + theme(legend.position="none")

fit_wafer(sim_curves,dev_curve=curve_4BARO)

## ----fig.width = 7,fig.height = 4----------------------------------------
sim_curves = NULL
t1 = rnorm(100,-10,1)
t2 = rnorm(100,-20,1)
t3 = rnorm(100,1,0.1)
t4 = rnorm(100,1,0.1)
for (i in 1:100) {
  simy = curve_4BARO(datax, c(t1[i],t2[i],t3[i],t4[i]));
  erry = rnorm(length(simy),0,1)
  simy = simy + erry
  sim_curves = rbind(sim_curves,data.frame(VG=datax,ID=exp(simy),name=as.character(i),
                wafer_id=1,id=1,ERF=1,row=1,col=1,comments="",test_num=1,test_date="1",
                result=-1))
}
ggplot(data=sim_curves, aes(x=VG, y=ID, group=name,color=name)) + geom_line() + scale_y_log10() +
  ylab("ID") + xlab("VG") + theme(legend.position="none")

fit_wafer(sim_curves,dev_curve=curve_4BARO)

## ----fig.width = 7,fig.height = 4----------------------------------------
sim_curves = NULL
t1 = rnorm(100,-10,2)
t2 = rnorm(100,-20,2)
t3 = rnorm(100,1,0.2)
t4 = rnorm(100,1,0.2)
for (i in 1:100) {
  simy = curve_4BARO(datax, c(t1[i],t2[i],t3[i],t4[i]));
  erry = rnorm(length(simy),0,5)
  simy = simy + erry
  sim_curves = rbind(sim_curves,data.frame(VG=datax,ID=exp(simy),name=as.character(i),
                wafer_id=1,id=1,ERF=1,row=1,col=1,comments="",test_num=1,test_date="1",
                result=-1))
}
ggplot(data=sim_curves, aes(x=VG, y=ID, group=name,color=name)) + geom_line() + 
  scale_y_log10() + ylab("ID") + xlab("VG") + theme(legend.position="none")

(fit = fit_wafer(sim_curves,dev_curve=curve_4BARO))
simy = curve_4BARO(datax, c(fit$X1[1],fit$X1[1],fit$X3[1],fit$X4[1]));
sim_curve = data.frame(datax=datax,simy=exp(simy))
ggplot(data=sim_curve, aes(x=datax, y=simy)) + geom_line() + scale_y_log10() +
  ylab("ID") + xlab("VG")

## ----fig.width = 7,fig.height = 4----------------------------------------
sim_curves = NULL
for (i in 1:100) {
  erry = rnorm(length(simy),1e-5,1e-4)
  simy = erry
  sim_curves = rbind(sim_curves,data.frame(VG=datax,ID=exp(simy),name=as.character(i),
                wafer_id=1,id=1,ERF=1,row=1,col=1,comments="",test_num=1,test_date="1",
                result=-1))
}
ggplot(data=sim_curves, aes(x=VG, y=ID, group=name,color=name)) + geom_line() + scale_y_log10() +
  ylab("ID") + xlab("VG") + theme(legend.position="none")

(fit = fit_wafer(sim_curves,dev_curve=curve_4BARO))


params = c(0.943965, 0.653355,  0.8080, 5.2859,  6.4161,  2.75493)
week = c(0.002624,  0.009514, -0.4339,  0.3307, -0.1511,  0.02401)
treatment = c(0.045662, -0.319895, -1.3305, -3.9086, -6.8740, -2.67339)
# y == ID
# x == VG

simulate = function(params, x=seq(-6, 6, length.out = 60), 
                    sd = 0.02) {
  
  forward = voltagefit:::logcurve(x, params) + rnorm(length(x), 0, sd)
  backward = voltagefit:::logcurve(x, params) + rnorm(length(x), 0, sd)
  
  c(forward, rev(backward))
}

simulate_wafer = function(params, ERF, direction, sd = 0.02, n=10) {
  
  params = params + rnorm(length(params), 0, sd=sd)
  dd = data.frame(id = ERF, name = 1, max = 1, cost =1,direction=direction, 
                  X1 = params[1], X2 = params[2], X3 = params[3],
                  X4 = params[4], X5 = params[5], X6 = params[6], stringsAsFactors = FALSE) 
  for(i in 2:n){
    params = params + rnorm(length(params), 0, sd=sd)
    dd_tmp = data.frame(id = ERF, name = 1, max = 1, cost =1, direction=direction,
                    X1 = params[1], X2 = params[2], X3 = params[3],
                    X4 = params[4], X5 = params[5], X6 = params[6], stringsAsFactors = FALSE) 
    dd = rbind(dd, dd_tmp)
  }
  dd
}

dd = rbind(simulate_wafer(params, as.character(1), direction="Forward"),
           simulate_wafer(params, as.character(1), direction="Backward"))

for(i in 2:8) {
  dd_tmp = rbind(simulate_wafer(params, as.character(i), direction="Forward"),
                 simulate_wafer(params, as.character(i), direction="Backward"))
  dd = rbind(dd, dd_tmp)
}

wafer = unique(dd$id)
week = c(1, 1, 1, 1, 2, 2, 2, 2)
treatment = 1:2
(design = data.frame(wafer = wafer, week = week, treatment = treatment))
fitted=dd
fit_manova(fitted, design)

plot(all)



for(i in 1:5) {
  base1 = data.frame(ERF = 1, name = 1, VG = simulate(params), ID = c(x, rev(x)))
  base2 = data.frame(ERF = 1, name = 1, VG = simulate(params), ID = c(x, rev(x)))
  base3 = data.frame(ERF = 1, name = 1, VG = simulate(params), ID = c(x, rev(x)))
  
  fit_wafer(dd)
  
  
  head(wafer)
  x = 
    voltagefit:::logcurve(x, baseline)
  
  data_dir = system.file("extdata/", package="voltagefit")
  wafer = readRDS(file.path(data_dir, "3737.rds"))
  
  name1 = wafer$name[1]
  ht(wafer[wafer$name==name1, ])
  tot_obs = length(wafer[wafer$name==unique(wafer$name)[1],]$VG) 
  tot_f = which(diff(wafer[wafer$name==unique(wafer$name)[1],]$VG) < 0)[1]
  
  w = wafer[wafer$name==name1, ]
  w = w[1:193,]
  plot(w$VG, w$ID, log="y")
  
  
  min_logcurve(c(1.0, 1.4, -3, 4, -1.5, 0.3), d_forward$VG,
               max(log(abs(d_forward$ID))) / log(abs(d_forward$ID)))
  
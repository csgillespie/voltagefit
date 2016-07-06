# plotting functions 

## function to plot curves for each week
##
## curves is of the form, weekcurve, 
## orig is whether the plot should be on the original data scale or 
## the transformed scale, default is the original scale, tot_obs is the 
## total number of observations made on each device within a wafer 
## (default=241), tot_f is the total number of observations made in 
## the forward pass on each device within a wafer (default=193)

plotweek = function(wafer, curves, orig=TRUE, tot_obs=241, tot_f=193)
{
  v_f=wafer$VG[1:tot_f]
  v_b=wafer$VG[tot_f:tot_obs]
  
  op = par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,cex.axis=0.9, las=1)
  on.exit(op)
  
  if(orig) {
    m = max(log(abs(wafer$ID)))
    curves$forward = exp(m/curves$forward)
    curves$backward = exp(m/curves$backward)
  }
  limits = range(curves$backward, curves$forward)
  
  plot(v_f,curves$forward[1,],type="l",xlab="Vg (V)",ylab="Id (A)",log="y", 
       ylim=limits,
       main=ifelse(orig, "", "Transformed scale"), panel.first=grid())
  
  for(i in 2:nrow(curves$forward)) {
    lines(v_f, curves$forward[i,], col=i)
  }
  
  for(i in 1:nrow(curves$backward)) {
    lines(v_b, curves$backward[i,], col=i, lty=2)
  }
  
  temp = paste(unique(wafer$test_date))
  legend("topleft", temp,col=1:length(unique(wafer$test_date)), lty=1)
  legend("bottomright", c("Forwards","Backwards"), col=1, lty=1:2)
}


## function to create underlying forward curve
##
## curves is of the form, undercurve
## orig is whether the plot should be on the original data scale or 
## the transformed scale, default is the original scale
## med is whether the mean or the median should be used, the default 
## is the mean curve, tot_obs is the total number of observations made 
## on each device within a wafer (default=241), tot_f is the total 
## number of observations made in the forward pass on each device 
## within a wafer (default=193)

plotunder = function(wafer,curves,orig=T,med=F,tot_obs=241,tot_f=193)
{
  v_f=wafer$VG[1:tot_f]
  v_b=wafer$VG[tot_f:tot_obs]
  
  if(orig==F)
  {
    if(med==F)
    {
      means_f=colMeans(curves$forward)
    }
    else
    {
      means_f=colQuantiles(curves$forward,probs=0.5)
    }
    lower_f=colQuantiles(curves$forward,probs=0.025)
    upper_f=colQuantiles(curves$forward,probs=0.975)
    
    if(med==F)
    {
      means_b=colMeans(curves$backward)
    }
    else
    {
      means_b=colQuantiles(curves$backward,probs=0.5)
    }
    lower_b=colQuantiles(curves$backward,probs=0.025)
    upper_b=colQuantiles(curves$backward,probs=0.975)

    par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,cex.axis=0.9, las=1)
    plot(v_f,means_f,type="l",ylim=c(0.3,1.1),log="y",main="Transformed scale",xlab="Vg (V)",ylab="Id (A)")
    grid()
    lines(v_f,lower_f,lty=2)
    lines(v_f,upper_f,lty=2)
    lines(v_b,means_b,col=2)
    lines(v_b,lower_b,lty=2,col=2)
    lines(v_b,upper_b,lty=2,col=2)
    if(med==F)
    {
      legend("topleft",c("Mean","95% Interval"),col=c(1,1),lty=c(1,2))
    }
    else
    {
      legend("topleft",c("Median","95% Interval"),col=c(1,1),lty=c(1,2))
    }
  }
  else
  {
    m = max(log(abs(wafer$ID)))
    
    if(med==F)
    {
      means_f=colMeans(exp(m/curves$forward))
    }
    else
    {
      means_f=colQuantiles(exp(m/curves$forward),probs=0.5)
    }
    lower_f=colQuantiles(exp(m/curves$forward),probs=0.025)
    upper_f=colQuantiles(exp(m/curves$forward),probs=0.975)
    
    if(med==F)
    {
      means_b=colMeans(exp(m/curves$backward))
    }
    else
    {
      means_b=colQuantiles(exp(m/curves$backward),probs=0.5)
    }
    lower_b=colQuantiles(exp(m/curves$backward),probs=0.025)
    upper_b=colQuantiles(exp(m/curves$backward),probs=0.975)
    
    par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,cex.axis=0.9, las=1)
    plot(v_f,means_f,type="l",ylim=c(2e-11,1e-03),log="y",main="",xlab="Vg (V)",ylab="Id (A)")
    grid()
    lines(v_f,lower_f,lty=2)
    lines(v_f,upper_f,lty=2)
    lines(v_b,means_b,col=2)
    lines(v_b,lower_b,lty=2,col=2)
    lines(v_b,upper_b,lty=2,col=2)
    if(med==F)
    {
      legend("topleft",c("Mean","95% Interval"),col=c(1,1),lty=c(1,2))
    }
    else
    {
      legend("topleft",c("Median","95% Interval"),col=c(1,1),lty=c(1,2))
    }
  }
}

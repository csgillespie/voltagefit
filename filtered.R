##FOR USE IN ORIGINAL SETUP, DOES NOT FEATURE IN PACKAGE AND CAN 
##BE REMOVED, FOR USE LOCALLY ON ORIGINAL FILES

## filtered function
## function to filter a weeks worth of wafers, tot_obs is the total 
## number of observations made on each device within a wafer (default=241). 
## dat, should be a data.frame containing all wafers for a week with 
## the same date stamp, e.g. "week 1". It should contain the columns 
## "wafer_id", "name", "test_date", "VG" and "ID"

## typical setup is
## library(splitstackshape) #for cSplit
## dat1 <- readRDS("ERF911_50x2um_curves.rds") 
## df <- cSplit(dat1, c("VG", "ID"), sep = ",", "long")
## df$test_date="week 1"
## test = filtered(df,241)
##

filtered = function(dat,tot_obs=241)
{
  temp1=NULL
  for(i in unique(dat$wafer_id))
  {
    dd = subset(dat,wafer_id %in% c(i))
    
    cond1 = subset(dd, VG<=-5.95 & abs(ID)>1e-09)
    dd_cond1=subset(dd, !(name %in% unique(cond1$name)))
    
    cond2 = subset(dd, VG>=5.95 & abs(ID)<1e-09)
    dd_cond2=subset(dd_cond1, !(name %in% unique(cond2$name)))
    
    # possible need to rewrite this, or combine with cond1
    cond3 = subset(dd, VG>-6 & VG< -4 & abs(ID)>1e-09)
    dd_cond3=subset(dd_cond2, !(name %in% unique(cond3$name)))
    
    res=subset(dd,VG>=5.95)
    cond4 = subset(res,(1/abs(ID))>1e6) #THIS TO CHECK
    dd_cond4=subset(dd_cond3, !(name %in% unique(cond4$name)))
    
    dd_filtered = dd_cond4
    
    temp=NULL
    for(j in unique(dd_filtered$name))
    {
      dd=subset(dd_filtered, name %in% c(j))
      temp=rbind(temp,dd[1:tot_obs,])
      if(length(dd$VG)!=tot_obs) print(paste("WARNING, unusual observation lengths for wafer ",i,", device ",j,sep=""))
    }
    temp1=rbind(temp1,temp)
  }
  return(temp1)
}

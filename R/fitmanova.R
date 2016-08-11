#' Perform the MANOVA parts of the analysis
#'
#' Perform the MANOVA parts of the analysis
#'
#' @param data     The data frame of parameter values, appended with week and treatment type for each device 
#' @param weight   The weights to be used in the MANOVA, (applied as 1/weight).
#' 
#' @return A list, a MANOVA class item (incorporating weights) and the output of vcov (without weights).
#' 
## TREATMENT NEEDS TO BE INCORPORATED!!! HOW?????
## N.B. vcov does not work on output which incorporates weights,
## the use of vcov helps with correlation structure when simulating 
## values for the underlying curve (otherwise odd curves were evident 
## from bizarre parameter combinations)
#'
man = function(data, weight){
  ## fit MANOVA with weight
  man_w = manova(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ factor(week), 
                   data = data, weights = 1 / weight) 
  ## calculate vcov (without weight)
  varcov = vcov(lm(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ factor(week), data = data))
  
  return(list(man_w = man_w, varcov = varcov))
}


#' Fit the results of fitwafer using MANOVA
#'
#' Fit the results of fitwafer using MANOVA
#'
#' @param fitted    Fitted data.frame as given by \code{\link{fitwafer}}.
#' @param design    The design matrix (of type data.frame with columns week, wafer, replicate, treatment).
#' 
#' @return A list with elements forward and backward, each containing the MANOVA output incorporating 
#' weights and vcov (excluding weights).
#'
#' @examples
#' wafers_folder = paste(path.package("voltagefit"),"/extdata/",sep="") # path to wafers data directory
#' fitted = fitall(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = c(1:6), treatment = rep(1:6))
#' fitman = fitmanova(fitted, design)
#'
#' @export
fitmanova = function(fitted, design){
  op = getOption("contrasts")
  options(contrasts = c("contr.sum", "contr.sum")) ## set contrasts to be used in MANOVA
  on.exit(options(contrasts = op))
  
  ## calculate the week and treatment set up from the design matrix, 
  ## replicating for all devices on a wafer
  len = tapply(fitted[fitted$direction=="Forward",]$id,fitted[fitted$direction=="Forward",]$id, length)
  week = rep(design[design$wafer==names(len),]$week, len)
  treatment = rep(design[design$wafer==names(len),]$treatment, len)
  
  ## combine with parameters
  fdata = data.frame(week = week, treatment = treatment, fitted[fitted$direction=="Forward",])
  bdata = data.frame(week = week, treatment = treatment, fitted[fitted$direction=="Backward",])
  
  # HOW TO GET TREATMENT IN TO BELOW WITHOUT IT BREAKING!!!!
  
  f = man(fdata, fdata$cost) ## fit MANOVA to forward pass
  b = man(bdata, fdata$cost) ## fir MANOVA to backward pass
  
  return(list(forward = f, backward = b))
}

#' Perform MANOVA analysis
#'
#' Performs a MANOVA analysis using curves fitted to wafers
#'
#' @param data     The data frame of parameter values, appended with week and treatment type for each device 
#' @param weight   The weights to be used in the MANOVA, (applied as 1/weight).
#' 
#' @return A list, a MANOVA class item (incorporating weights) and the
#' output of vcov (without weights).
#' 
## TREATMENT NEEDS TO BE INCORPORATED!!! HOW?????
## N.B. vcov does not work on output which incorporates weights,
## the use of vcov helps with correlation structure when simulating 
## values for the underlying curve (otherwise odd curves were evident 
## from bizarre parameter combinations)
#'
param_manova = function(data, weight){
  ## fit MANOVA with weight
  man_w = manova(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ factor(week) , 
                 data = data, weights = 1 / weight) 
  ## calculate vcov (without weight)
  varcov = vcov(lm(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ factor(week), 
                   data = data))
  return(list(man_w = man_w, varcov = varcov))
}

#' Perform MANOVA analysis using fitted curves
#'
#' Performs a MANOVA analysis using curves that have been fitted
#' to wafers, using \code{\link{fit_wafer}}.
#'
#' @param fitted    Fitted data.frame as given by \code{\link{fit_wafer}}.
#' @param design    The design matrix (of type data.frame with columns week, wafer, replicate, treatment).
#' 
#' @return A list with elements forward and backward, each containing
#' the MANOVA output incorporating weights and vcov (excluding weights).
#'
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fitted = fit_all(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), 
#'               replicate = 1:6, treatment = rep(1,6))
#' fitman = fit_manova(fitted, design)
#'
#' @export
fit_manova = function(fitted, design){
  op = getOption("contrasts")
  options(contrasts = c("contr.sum", "contr.sum")) ## set contrasts to be used in MANOVA
  on.exit(options(contrasts = op))
  
  ## Order data frames to make life easy
  design = design[order(design$wafer),]
  fitted = fitted[order(fitted$id),]
  
  ## calculate the week and treatment set up from the design matrix, 
  ## replicating for all devices on a wafer
  len = table(fitted$id)/2 # forward/backward
  week = factor(rep(design[design$wafer == names(len),]$week, len))
  treatment = factor(rep(design[design$wafer==names(len),]$treatment, len))
  
  ## combine with parameters
  fdata = data.frame(week = week, treatment = treatment, fitted[fitted$direction=="Forward",])
  bdata = data.frame(week = week, treatment = treatment, fitted[fitted$direction=="Backward",])
  
  # HOW TO GET TREATMENT IN TO BELOW WITHOUT IT BREAKING!!!!
  
  f = param_manova(fdata, weight = fdata$cost) ## fit MANOVA to forward pass
  b = param_manova(bdata, weight = fdata$cost) ## fit MANOVA to backward pass
  
  return(list(forward = f, backward = b))
}

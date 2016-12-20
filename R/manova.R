## N.B. vcov does not work on output which incorporates weights,
## the use of vcov helps with correlation structure when simulating 
## values for the underlying curve (otherwise odd curves were evident 
## from bizarre parameter combinations)
param_manova = function(data, weight, week=TRUE, treatment=TRUE){
  
  if(week && treatment) {
    ## fit MANOVA with weight
    man_w = manova(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ week + treatment, 
                   data = data, weights = 1 / weight, 
                   contrasts = list(treatment = "contr.treatment", week = "contr.sum")) 
 
    ## calculate vcov (without weight)
    varcov = vcov(lm(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ week + treatment, 
                     data = data, 
                     contrasts = list(treatment = "contr.treatment", week = "contr.sum")))
  } else if(week) {
    man_w = manova(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ week , 
                   data = data, weights = 1 / weight, 
                   contrasts = list(week = "contr.sum")) 
    varcov = vcov(lm(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ week, 
                     data = data, 
                     contrasts = list(week = "contr.sum")))
  } else {
    man_w = manova(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ treatment, 
                   data = data, 
                   contrasts = list(treatment = "contr.treatment")) 
    
    ## calculate vcov (without weight)
    varcov = vcov(lm(data.matrix(data[,grep("X[0-9]+",colnames(data))]) ~ treatment, 
                     data = data, 
                     contrasts = list(treatment = "contr.treatment")))
    #varcov = vcov(man_w)
  }
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
#' @export
fit_manova = function(fitted, design){
  ## Order data frames to make life easy
  design = design[order(design$wafer),]
  fitted = fitted[order(fitted$id),]
  
  ## calculate the week and treatment set up from the design matrix, 
  ## replicating for all devices on a wafer
  len = table(fitted$id)/2 # forward/backward
  week = factor(rep(design[design$wafer == names(len),]$week, len))
  treatment = factor(rep(design[design$wafer==names(len),]$treatment, len))
  
  ## combine with parameters
  f_data = data.frame(week = week, treatment = treatment, fitted[fitted$direction=="Forward",])
  b_data = data.frame(week = week, treatment = treatment, fitted[fitted$direction=="Backward",])
  
  is_week = length(levels(week)) > 1; is_treatment = length(levels(treatment)) > 1
  data = f_data
  f = param_manova(f_data, weight = f_data$cost, week = is_week, treatment = is_treatment) ## fit MANOVA to forward pass
  b = param_manova(b_data, weight = f_data$cost, week = is_week, treatment = is_treatment) ## fit MANOVA to backward pass
  
  l = list(forward = f, backward = b, 
           no_week = length(levels(week)), 
           no_treatment = length(levels(treatment)))
  attr(l, "dev_curve") = attr(fitted, "dev_curve")
  class(l) = "fit_manova"
  return(l)
}

# Simulate new curves from parameters
simulate = function(x, n, direction) {
  
  (samples = mvrnorm(n, as.numeric(coefficients(x$man_w)), x$varcov))
  baseline = samples[,grep("Intercept", colnames(samples))]
  week = samples[,grep("week", colnames(samples))]
  treatment = samples[, grep("treatment", colnames(samples))]
  
  dd = as.data.frame(baseline)
  colnames(dd) = paste0("X", 1:ncol(dd))
  dd$type = "baseline"
  
  if(ncol(week) > 0) {
    dd_tmp = as.data.frame(week)
    colnames(dd_tmp) = paste0("X", 1:ncol(dd_tmp))
    dd_tmp$type = "week"
    dd = rbind(dd, dd_tmp)
  }
  
  if(ncol(treatment) > 0 ) {
    treatment = treatment + baseline
    
    dd_tmp = as.data.frame(treatment)
    colnames(dd_tmp) = paste0("X", 1:ncol(dd_tmp))
    dd_tmp$type = "treatment"
    dd = rbind(dd, dd_tmp)
  }
  dd$direction = direction
  dd$no_sims = 1:nrow(dd)
  return(dd)
}

sample_parameters = function(fm, n){
  forward = simulate(fm$forward, n, "Forward")
  backward = simulate(fm$backward, n, "Backward")
  sims = rbind(forward, backward)
  sims  
}

#' Sample underlying curves
#'
#' Generate sample forward and backward underlying curves.
#'
#' @param x    Data.frame as output by \code{\link{fit_manova}}.
#' @param n         Number of samples required (Default: 100).
#' @param ... Other arguments (not typically needed).
#' 
#' @return A data.frame consisting of the fields:
#'   \describe{
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{X1 ... X6}{The parameters characterising the curve}
#'      \item{type}{Baseline, treatment, or week.}
#'   }
#'   
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fitted = fit_all(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), 
#'         replicate = 1:6, treatment = rep(1,6))
#' fitman = fit_manova(fitted, design)
#' samples = sample(fitman)
#' @export
sample = function(x, n, ...)
  UseMethod("sample")

#' @export
sample.default = function(x, ...) base::sample(x, ...)

#' @export
sample.fit_manova = function(x, n=100, ...){
  (pars = sample_parameters(x, n))
  par_loc = get_par_loc(pars)
  #pars = pars[pars$type == "treatment",]
  dd = NULL
  i = 1
  for(i in seq_len(nrow(pars))) {
    dd_tmp = get_curve(pars[i, par_loc])
    dd_tmp$type = pars$type[i]
    dd_tmp$direction = pars$direction[i]
    dd_tmp$sim_no = i
    dd = rbind(dd, dd_tmp)
  }
  list(samples=dd, pars = pars)
}





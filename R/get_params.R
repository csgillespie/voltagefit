#' Calculate curve parameters for each week
#'
#' Calculate the parameters characterising both the forward and backward curves for
#' each week present in a fitted manova, provided by \code{\link{fit_manova}}.
#'
#' @param fm  Fitted manova list, as output by \code{\link{fit_manova}}.
#' @param combine Should the treatment parameters be combined (default TRUE).
#' 
#' @return A data.frame consisting of the fields:
#'   \describe{
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{week}{The week to which the curve corresponds}
#'      \item{X1 ... X6}{The parameters characterising the week curve}
#'   }
#' @export
get_params = function(fm, combine=FALSE) {
  baseline = baseline_params(fm)
  week = week_params(fm) 
  treatment = treatment_params(fm)

  if(combine){
    par_loc = get_par_loc(treatment)
    treatment[,par_loc] = treatment[, par_loc] + baseline[, par_loc]
  }
  rbind(baseline, week, treatment)
}

week_params = function(fm) {
  if(fm$no_week == 1) return(NULL)
  no_pars = ncol(fm$forward$man_w$coefficients)
  param_f = matrix(0, nrow = fm$no_week, ncol = no_pars) 
  param_b = param_f  
  
  for_coefs = fm$forward$man_w$coefficients
  back_coefs = fm$backward$man_w$coefficients
  i = 1
  
  ## Include intercept - 1
  weeks =  c(1, grep("week", rownames(for_coefs)))
  param_f[weeks[-1]-1, ] = for_coefs[1,] + for_coefs[weeks[-1],]
  param_b[weeks[-1]-1, ] = back_coefs[1,] + back_coefs[weeks[-1],]  

  ## Calculate the last parameter from the sum of all the others
  param_f[nrow(param_f), ] = for_coefs[1, ] - colSums(for_coefs[weeks[-1],, drop=FALSE])
  param_b[nrow(param_f), ] =  back_coefs[1, ] - colSums(back_coefs[weeks[-1],, drop=FALSE])
 
  params = as.data.frame(rbind(param_f, param_b))
  colnames(params) = dimnames(for_coefs)[[2]]
  rownames(params) = NULL
  params$type = paste0("week", seq_len(length(weeks))-1)
  params$direction = rep(c("Forward", "Backward"), each=length(weeks))
  return(params)
}

treatment_params = function(fm) {
  if(fm$no_treatment == 1) return(NULL)
  no_pars = ncol(fm$forward$man_w$coefficients)
  
  for_coefs = fm$forward$man_w$coefficients
  back_coefs = fm$backward$man_w$coefficients
  
  trt_rows = grep("treatment", rownames(for_coefs))
  param_f = for_coefs[trt_rows,]
  param_b = back_coefs[trt_rows,]
  params = as.data.frame(rbind(param_f, param_b))
  rownames(params) = NULL
  params$type = dimnames(for_coefs)[[1]][-1]
  params$direction = rep(c("Forward", "Backward"), each=length(trt_rows))
  params
}

baseline_params = function(fm) {
  params = rbind(fm$forward$man_w$coefficients[1,, drop=FALSE],  fm$backward$man_w$coefficients[1,, drop=FALSE])
  params = as.data.frame(params)
  rownames(params) = NULL
  params$type = "baseline"
  params$direction = c("Forward", "Backward")
  params
}

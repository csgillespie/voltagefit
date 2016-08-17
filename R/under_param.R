#' Calculate underlying curve parameters
#'
#' Calculates the parameters and covariance matrix that charactarises
#' both the forward and backward underlying curves, using a fitted manova
#' provided by \code{\link{fit_manova}}.
#' 
#' @inheritParams week_param
#' 
#' @return A list with elements \code{param} and \code{var}, where param is a data.frame consisting of the fields:
#'   \describe{
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{X1 ... X6}{The parameters characterising the curve}
#'   }
#'   and \code{var} is a list with elements forward and backward, each containing the covariance matrix for use in \code{\link{sample_under_param}}.
#'   
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fitted = fit_all(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), 
#'       treatment = rep(1,6))
#' fitman = fit_manova(fitted, design)
#' underp = under_param(fitman)
#'
#' @export
under_param = function(fm){
  t_all_f = fm$forward$man_w$coefficients[1,]  ## get underlying parameters from MANOVA
  t_all_b = fm$backward$man_w$coefficients[1,]

  loc = 1 + (0:5)*(length(fm$forward$man_w$xlevels$`factor(week)`))  ## index which elements in vcov we require

  t_vmat_f = matrix(0, ncol = 6, nrow = 6)
  t_vmat_b = t_vmat_f
  
  for (i in 1:6){
    t_vmat_f[i,] = fm$forward$varcov[loc[i], loc]  ## extract the elements of vcov we need
    t_vmat_b[i,] = fm$backward$varcov[loc[i], loc]
  }  
  
  forward = data.frame(direction="Forward", t(t_all_f))
  backward = data.frame(direction="Backward", t(t_all_b))
  
  return(list(param = rbind(forward, backward), var = list(forward = t_vmat_f, backward = t_vmat_b)))
}

#' Sample underlying curve parameters
#'
#' Generate sample forward and backward underlying curve parameters,
#' using the underlying curve parameters and covariance matrix given
#' by \code{\link{under_param}}.
#'
#' @param underp    Data.frame as output by \code{\link{under_param}}.
#' @param n         Number of samples required (Default: 1000).
#' 
#' @return A data.frame consisting of the fields:
#'   \describe{
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{X1 ... X6}{The parameters characterising the curve}
#'   }
#'   
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fitted = fit_all(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), 
#'         replicate = 1:6, treatment = rep(1,6))
#' fitman = fit_manova(fitted, design)
#' underp = under_param(fitman)
#' sample_under_param(underp)
#'
#' @export
sample_under_param = function(underp, n=1000){
  ## generate sample from multivariate Normal
  t_f_sim = mvrnorm(n, as.numeric(underp$param[underp$param$direction=="Forward",][,2:7]), underp$var$forward)
  t_b_sim = mvrnorm(n, as.numeric(underp$param[underp$param$direction=="Backward",][,2:7]), underp$var$backward)
  
  forward = data.frame(direction="Forward", t_f_sim)
  backward = data.frame(direction="Backward", t_b_sim)
  
  return(rbind(forward, backward))
}

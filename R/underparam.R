#' Calculate parameters for creating the underlying curve
#'
#' Calculate parameters for creating the underlying curve
#'
#' @param fm    Fitted manova list as output by \code{\link{fitmanova}}.
#' 
#' @return A list with elements param and var.\cr\cr
#'   param - A data.frame consisting of the fields:\cr
#'   direction - Forward or backward curve.\cr
#'   X1 ... X6 - The parameters characterising the week curves.\cr\cr
#'   var - A list with elements forward and backward, each containing the covariance matrix for use in \code{\link{undercurvesim}}.
#' @examples
#' wafers_folder = paste(path.package("voltagefit"),"/extdata/",sep="") # path to wafters data directory
#' fitted = fitall(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = c(1:6), treatment = rep(1:6))
#' fitman = fitmanova(fitted, design)
#' underp = underparam(fitman)
#'
#' @export
underparam = function(fm)
{
  t_all_f = fm$forward$man_w$coefficients[1,]  ## get underlying parameters form MANOVA
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



#' Create sample used for underlying curves
#'
#' Create sample used for underlying curves
#'
#' @param underp    Data.frame as output by \code{\link{underparam}}.
#' @param n    Number of samples required (default=1000).
#' 
#' @return A data.frame consisting of the fields:\cr
#'   direction - Forward or backward curve.\cr
#'   X1 ... X6 - The parameters characterising the curves.
#' @examples
#' wafers_folder = paste(path.package("voltagefit"),"/extdata/",sep="") # path to wafters data directory
#' fitted = fitall(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = c(1:6), treatment = rep(1:6))
#' fitman = fitmanova(fitted, design)
#' underp = underparam(fitman)
#' undercurvesim(underp)
#'
#' @export
undercurvesim = function(underp, n=1000)
{
  ## generate sample from multivariate Normal
  t_f_sim = mvrnorm(n, as.numeric(underp$param[underp$param$direction=="Forward",][,2:7]), underp$var$forward)
  t_b_sim = mvrnorm(n, as.numeric(underp$param[underp$param$direction=="Backward",][,2:7]), underp$var$backward)
  
  forward = data.frame(direction="Forward", t_f_sim)
  backward = data.frame(direction="Backward", t_b_sim)
  
  return(rbind(forward, backward))
}

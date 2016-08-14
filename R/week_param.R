#' Calculate curve parameters for each week
#'
#' Calculate the parameters characterising both the forward and backward curves for
#' each week present in a fitted manova, provided by \code{\link{fit_manova}}.
#'
#' @param fm  Fitted manova list, as output by \code{\link{fit_manova}}.
#' 
#' @return A data.frame consisting of the fields:
#'   \describe{
#'      \item{direction}{Whether the curve direction is forward or backward}
#'      \item{week}{The week to which the curve corresponds}
#'      \item{X1 ... X6}{The parameters characterising the week curve}
#'   }
#'   
#' @examples
#' wafers_folder = file.path(path.package("voltagefit"),"extdata") # path to wafers data directory
#' fitted = fit_all(wafers_folder)
#' design = data.frame(week = c(1,1,1,1,2,2), wafer = unique(fitted$id), replicate = 1:6, treatment = rep(1,6))
#' fitman = fit_manova(fitted, design)
#' weekp = week_param(fitman)
#'
#' @export
week_param = function(fm){
  param_f = matrix(0, nrow = length(fm$forward$man_w$xlevels$`factor(week)`), ncol = ncol(fm$forward$man_w$coefficients)) 
  param_b = param_f  
  
  for (i in 1:6){
    for (j in 1:(nrow(param_f) - 1)){
      ## for each parameter, extract and combine from MANOVA output. Both forward and backward 
      param_f[j,i] = fm$forward$man_w$coefficients[1,i] + fm$forward$man_w$coefficients[j+1,i]
      param_b[j,i] = fm$backward$man_w$coefficients[1,i] + fm$backward$man_w$coefficients[j+1,i]
    }
    ## calculate the last parameter from the sum of all the others
    param_f[nrow(param_f),i] = fm$forward$man_w$coefficients[1,i] - sum(fm$forward$man_w$coefficients[2:nrow(param_f),i])
    param_b[nrow(param_f),i] = fm$backward$man_w$coefficients[1,i] - sum(fm$backward$man_w$coefficients[2:nrow(param_f),i])
  }
  
  forward = data.frame(direction="Forward", week = fm$forward$man_w$xlevels$`factor(week)`, param_f)
  backward = data.frame(direction="Backward", week = fm$forward$man_w$xlevels$`factor(week)`, param_b)
  
  return(rbind(forward,backward))
}

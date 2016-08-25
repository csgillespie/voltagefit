get_curve = function(pars, x = seq(-4, 4, length.out=100)) {
  pars = as.numeric(pars)
  y = logcurve(x, pars)
  data.frame(x = x, y = exp(y), stringsAsFactors = FALSE)
}


#' @export
mean.fit_manova = function(x, direction="Forward", ...){
  pars = get_params(x, combine=TRUE)
  #pars = pars[pars$direction == direction, , drop=FALSE]
  
  par_loc = get_par_loc(pars)
  dd = NULL
  for(i in seq_len(nrow(pars))) {
    dd_tmp = get_curve(pars[i, par_loc])
    dd_tmp$type = pars$type[i]
    dd_tmp$direction = pars$direction[i]
    dd = rbind(dd, dd_tmp)
  }
  dd
}
# 
# 
# 
# means = mean(fit_man, direction="Forward")
# 
# library(ggplot2)
# ggplot(means) + geom_line(aes(x,y, colour=type)) + 
#   scale_y_log10()

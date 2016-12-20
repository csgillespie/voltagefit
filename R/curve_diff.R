curve_diff = function(x, type, direction="Forward") {
  
  m1 = x[x$type == type[1] & x$direction == direction, ]
  m2 = x[x$type == type[2] & x$direction == direction, ]
  if(is.null(x$sim)) {
    data.frame(VG = m1$VG, diff = log10(m1$ID) - log10(m2$ID))  
    
  } else {
    message("sim")
    m1 = m1[order(m1$sim),]
    m2 = m2[order(m2$sim),]
    data.frame(VG = m1$VG, diff = log10(m1$ID) - log10(m2$ID), sim = m1$sim)
  }
}

#' @inheritParams mean.fit_manova
#' @export
curve_diff_mean = function(x, type, direction="Forward") {
  means = mean(x)  
  curve_diff(means, type, direction)
}

#' @export
curve_diff_sample = function(x, type, n, direction="Forward") {
  unders = sample(x,  n=n)$samples
  curve_diff(unders, type, direction)
}


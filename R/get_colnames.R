# return the location of the colnames
get_par_loc = function(fit, location = TRUE) {
  col_names = colnames(fit)
  loc = grep("^X[0-9]+$", col_names)  
  if(!location) {
    loc = col_names[loc]
  }
  loc
}

get_num_pars = function(fit) 
  length(get_par_loc(fit))



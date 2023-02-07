dist_mat_to_covariates = function(tib, mat_var){
  first_mat = tib[[mat_var]][[1]]
  col_combos = ncol(first_mat) %>% combn(2) %>% t()
  col_names = colnames(first_mat)

  for(i in 1:nrow(col_combos)){
    r = col_combos[i,1]
    c = col_combos[i,2]
    # browser()
    cov_name = paste0(col_names[r], "~", col_names[c])
    tib[cov_name] = map_dbl(tib[[mat_var]], \(x) x[r, c])
  }
  return(tib)
}

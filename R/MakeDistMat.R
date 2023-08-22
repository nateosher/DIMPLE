#' Converts intensity matrix (i.e. matrix where columns represent cell types,
#' rows represent locations, and values represent smoothed intensities) into
#' a distance matrix with specified distance metric.
#' @param intensity_mat Intensity matrix
#' @param dist_metric A distance metric between intensities;  Any function that
#' takes in two vectors of equal length and produces a scalar
#' @return Distance matrix with rows/columns named appropriately
MakeDistMat = function(intensity_mat, dist_metric, symmetric=T){
  if(symmetric==F){
    col_combos = tibble(x1=1:ncol(intensity_mat),x2=1:ncol(intensity_mat)) %>% 
      expand(x1, x2) %>%
      filter(x1!=x2) %>% 
      as.matrix()
  }else{
    col_combos = ncol(intensity_mat) %>% combn(2) %>% t()
  }
  dist_mat = matrix(NA, nrow = ncol(intensity_mat), ncol = ncol(intensity_mat))
  dists = map_dbl(1:nrow(col_combos), \(i){
    dist_metric(intensity_mat[,col_combos[i, 1]],
                intensity_mat[,col_combos[i, 2]])
  })
  dist_mat[col_combos] = dists
  colnames(dist_mat) = colnames(intensity_mat)
  rownames(dist_mat) = colnames(intensity_mat)
  dist_mat
}

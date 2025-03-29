#' Converts intensity matrix (i.e. matrix where columns represent cell types,
#' rows represent locations, and values represent smoothed intensities) into
#' a distance matrix with specified distance metric.
#' @param intensity_mat Intensity matrix
#' @param dist_metric A distance metric between intensities;  Any function that
#' takes in two vectors of equal length and produces a scalar
#' @return Distance matrix with rows/columns named appropriately
MakeDistMat = function(intensity_mat, dist_metric, symmetric=T){
  # If there is only one cell type, return 1 x 1 matrix with NA entry
  if(ncol(intensity_mat) == 1){
    return(
      matrix(NA, nrow = 1, ncol = 1) %>%
        magrittr::set_rownames(colnames(intensity_mat)) %>%
        magrittr::set_colnames(colnames(intensity_mat))
    )
  }

  if(!symmetric){
    col_combos = tibble::tibble(x1=1:ncol(intensity_mat),x2=1:ncol(intensity_mat)) %>%
      tidyr::expand(x1, x2) %>%
      dplyr::filter(x1!=x2) %>%
      as.matrix()
  }else{
    col_combos = ncol(intensity_mat) %>% combn(2) %>% t()
  }
  dist_mat = matrix(NA, nrow = ncol(intensity_mat), ncol = ncol(intensity_mat))
  dists = map_dbl(1:nrow(col_combos), \(i){
    tryCatch({
      dist_metric(intensity_mat[,col_combos[i, 1]],
                  intensity_mat[,col_combos[i, 2]])
    }, error = function(e) NA)
    # dist_metric(intensity_mat[,col_combos[i, 1]],
    #             intensity_mat[,col_combos[i, 2]])
  })
  dist_mat[col_combos] = dists
  colnames(dist_mat) = colnames(intensity_mat)
  rownames(dist_mat) = colnames(intensity_mat)
  dist_mat
}

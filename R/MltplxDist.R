#' Creates `MltplxDist` object.
#' @param mltplx_intensity Intensity matrix (i.e. matrix where columns represent cell types,
#' rows represent locations, and values represent smoothed intensities)
#' @param dist_metric A distance metric between intensities;  Any function that
#' takes in two vectors of equal length and produces a scalar
#' @param .dist_metric_name String representing the name of the distance metric
#' used
#' @return `MltplxDist` object
#' @export
new_MltplxDist = function(mltplx_intensity, dist_metric, .dist_metric_name,
                          symmetric = TRUE){
  intensities <- mltplx_intensity$intensities
  distance_matrix = MakeDistMat(intensities[,!(colnames(intensities) %in% c("X","Y"))],
                                dist_metric, symmetric = symmetric)

  structure(
    list(
      dist = distance_matrix,
      metric = .dist_metric_name,
      cell_types = colnames(distance_matrix)
    ),
    class = "MltplxDist"
  )
}

#' @export
print.MltplxDist = function(dist){
  cat("MltplxDist object with", length(dist$cell_types), "cell types\n")
  cat("Distance metric:", dist$metric, "\n")
  cat("\n")
  print(dist$dist)
}

#' Creates new `MltplxIntensity` object.
#' @param mltplx_image `MltplxImage` object
#' @param ps "Pixel size," i.e. the size of the squares that the domain will
#' be broken up into in order to estimate the intensities of various cell types.
#' @param bw Bandwidth- intensity estimates are smoothed, and larger bandwidths
#' result in more smoothing.
#' @return `MltplxIntensity` object
#' @export
new_MltplxIntensity = function(mltplx_image, ps, bw){
  intensity_object = pat.density(mltplx_image$ppp, eps = ps, sigma = bw)
  intensity_matrix = intensity_object$dens %>%
    mutate(X = intensity_object$X,
           Y = intensity_object$Y) %>%
    as.matrix()


  structure(
    list(
      intensities = intensity_matrix,
      cell_types = setdiff(colnames(intensity_matrix),c("X","Y")),
      ps = ps,
      bw = bw,
      dim = intensity_object$dim
    ),
    class = "MltplxIntensity"
  )
}

#' @export
print.MltplxIntensity = function(mltplx_intensity){
  cell_type_str = paste0(mltplx_intensity$cell_types, collapse = ", ")
  cat("MltplxIntensity object with", length(mltplx_intensity$cell_types),
      "cell types\n")
  cat(length(mltplx_intensity$cell_types), "cell types:", cell_type_str, "\n")
  cat("Pixel size:", mltplx_intensity$ps, "\n")
  cat("Bandwidth:", mltplx_intensity$bw, "\n")
  cat("Grid dimensions:", mltplx_intensity$dim[1], "x",
      mltplx_intensity$dim[1], "\n")
}

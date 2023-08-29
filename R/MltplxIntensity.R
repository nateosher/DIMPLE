#' Creates new `MltplxIntensity` object.
#' @param mltplx_image `MltplxImage` object
#' @param ps "Pixel size," i.e. the size of the squares that the domain will
#' be broken up into in order to estimate the intensities of various cell types.
#' @param bw Bandwidth- intensity estimates are smoothed, and larger bandwidths
#' result in more smoothing.
#' @return `MltplxIntensity` object
#' @export
new_MltplxIntensity = function(mltplx_image, ps, bw){
  intensity_object = pat.density(mltplx_image$ppp, eps = ps, sigma = bw, positive = TRUE)
  intensity_matrix = intensity_object$dens %>%
    mutate(X = intensity_object$X,
           Y = intensity_object$Y) %>%
    drop_na() %>% # this handles holes
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

#' @export
plot.MltplxIntensity = function(mltplx_intensity, ...){
  args = list(...)
  if(is.null(args$types)){
    types = mltplx_intensity$cell_types
  }else{
    types = args$types
  }

  d = mltplx_intensity$intensities %>%
    tibble::as_tibble() %>%
    dplyr::select(all_of(types),X,Y) %>%
    tidyr::pivot_longer(-c(X,Y),names_to = "type",values_to = "intensity")

  ggplot(d, aes(X,Y,fill=intensity)) +
    geom_tile() +
    facet_wrap(~type) +
    viridis::scale_fill_viridis() +
    ggtitle(paste0("Cell intensities")) +
    xlab("") + ylab("") +
    guides(fill = guide_colorbar(title = "Intensity")) +
    theme(
      axis.text.x = element_text(face="bold", colour = "black"),
      axis.text.y = element_text(face="bold", colour = "black")
    )
}

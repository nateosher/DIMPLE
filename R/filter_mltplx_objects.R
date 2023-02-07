#' Filters `MltplxExperiment` object based on IDs of slides
#' @param mltplx_experiment `MltplxExperiment` object to filter
#' @param slide_ids Vector of slide ids to be kept in result `MltplxExperiment`
#' @return List of `MltplxObject`s with specified ids
#' @export
filter_mltplx_objects <- function(mltplx_experiment, slide_ids) {
  idx <- unlist(lapply(mltplx_experiment$mltplx_objects,
                       \(obj) obj$slide_id %in% slide_ids))
  mltplx_experiment[idx]
}

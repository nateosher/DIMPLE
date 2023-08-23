#' Update the estimated distances between intensities for all cell types of
#' all slides of a `MltplxExperiment` object, or creates them if there are none.
#' @param mltplx_experiment The `MltplxExperiment` object whose distance
#' matrices you wish to update
#' @param dist_metric The distance metric you'd like to use to compute distance
#' between vectorized intensities. This can be any function that takes in two
#' vectors of equal length and returns a scalar.
#' @return Updated `MltplxExperiment` object
#' @importFrom magrittr `%>%`
#' @importFrom purrr imap
#' @export
update_dist = function(mltplx_experiment, dist_metric,
                       symmetric = TRUE, ...){
  mltplx_objects <- mltplx_experiment$mltplx_objects
  .dist_metric_name = substitute(dist_metric) %>% as.character()
  total_objects = length(mltplx_objects)

  progressr::with_progress({
    prog <- progressr::progressor(steps = total_objects)

    mltplx_objects <- furrr::future_map(mltplx_objects, \(obj){
      obj <- update_object_dist(obj,
                                dist_metric = dist_metric,
                                .dist_metric_name = .dist_metric_name,
                                symmetric = symmetric)
      prog()
      return(obj)
    })
  })

  mltplx_experiment$mltplx_objects <- mltplx_objects
  mltplx_experiment$dist_metric_name = .dist_metric_name
  return(mltplx_experiment)
}

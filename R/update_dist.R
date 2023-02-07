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
update_dist = function(mltplx_experiment, dist_metric){
  total_objects = length(mltplx_experiment$mltplx_objects)
  .dist_metric_name = substitute(dist_metric) %>% as.character()
  mltplx_experiment$mltplx_objects = mltplx_experiment$mltplx_objects %>%
    imap(\(obj, i){
      ProgressBar(i, total_objects)
      update_object_dist(obj, dist_metric = dist_metric,
                         .dist_metric_name = .dist_metric_name)
    })
  ProgressBar(total_objects + 1, total_objects)
  mltplx_experiment$dist_metric_name = .dist_metric_name
  return(mltplx_experiment)
}

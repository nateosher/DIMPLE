#' Update the estimated intensities for all slides of a `MltplxExperiment`
#' object, or creates them if there are none.
#' @param mltplx_experiment `MltplxExperiment` object whose intensities you wish
#' to update
#' @param ps "Pixel size" of the intensity estimations. Each slide will be
#' broken up into squares of size approximately `ps` by `ps` units.
#' @param bw Bandwidth of the smoothing of the intensity estimations. Larger
#' values lead to more smoothing. Note that more smoothing is not necessarily
#' better, and there is no single correct value for smoothing.
#' @return Updated `MltplxExperiment` object
#' @importFrom magrittr `%>%`
#' @export
update_intensity = function(mltplx_experiment, ps, bw){
  total_objects = length(mltplx_experiment$mltplx_objects)
  mltplx_experiment$mltplx_objects = mltplx_experiment$mltplx_objects %>%
    purrr::imap(\(obj, i){
      ProgressBar(i, total_objects)
      update_object_intensity(obj, ps = ps, bw = bw)
    })
  ProgressBar(total_objects + 1, total_objects)
  mltplx_experiment$ps = ps
  mltplx_experiment$bw = bw
  return(mltplx_experiment)
}

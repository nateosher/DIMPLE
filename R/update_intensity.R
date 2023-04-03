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
  mltplx_objects <- mltplx_experiment$mltplx_objects

  total_objects = length(mltplx_objects)
  
  progressr::with_progress({
    prog <- progressr::progressor(steps = total_objects)
    
    mltplx_objects <- furrr::future_map(mltplx_objects, \(obj){
        obj <- update_object_intensity(obj, ps = ps, bw = bw)
        prog()
        return(obj)
    })
  })
  
  mltplx_experiment$mltplx_objects <- mltplx_objects
  mltplx_experiment$ps = ps
  mltplx_experiment$bw = bw
  return(mltplx_experiment)
}

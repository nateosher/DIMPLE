#' Given a MltplxExperiment object and a vector of ids,
#' produces a new MltplxExperiment object containing only slides whose
#' ids are in the id vector.
#' @param mltplx_experiment MltplxExperiment object to be filtered
#' @param id_list vector of ids to keep in resulting MltplxExperiment object
#' @return New MltplxExperiment object containing only specified slides
#' @export
filter_exp = function(mltplx_experiment, id_list){
  if(class(mltplx_experiment) != "MltplxExperiment")
    stop("`mltplx_experiment` argument must be of class `MltplxExperiment`")
  filtered_exp = mltplx_experiment
  filtered_exp$mltplx_objects = purrr::keep(filtered_exp$mltplx_objects,
                                            ~ .x$slide_id %in% id_list)
  if(!is.null(filtered_exp$metadata)){
    filtered_exp$metadata = filtered_exp$metadata %>%
      dplyr::filter(slide_id %in% id_list)
  }
  if(length(filtered_exp$mltplx_objects) == 0)
    warning("resulting `MltplxExperiment` has no slides")

  return(filtered_exp)
}

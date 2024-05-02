#' Plots intensities for selected cell types and selected slides of
#' `MltplxExperiment` object.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param slide_ids Vector of slide ids for which you would like to plot
#' the selected cell intensities.
#' @param types Vector of cell types whose intensities you'd like to plot
#' @return list of ggplot2 objects
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_intensity_surface.MltplxExperiment <- function(mltplx_experiment,slide_ids,types) {
  if(is.null(mltplx_experiment[[1]]$mltplx_intensity))
    stop("intensities have not been generated for this MltplxExperiment")

  filtered_exp <- filter_MltplxExp(mltplx_experiment,slide_ids)

  all_cell_types = map(filtered_exp$mltplx_objects, ~ .x$mltplx_image$cell_types) %>%
    unlist() %>%
    unique()

  all_slide_ids = map_chr(filtered_exp$mltplx_objects, ~ .x$slide_id)

  if(!any(slide_ids %in% all_slide_ids))
    stop("none of the slide ids passed as arguments are present in `MltplxExperiment` object")

  if(!any(types %in% all_cell_types))
    stop("none of the cell types passed as arguments are present in subset of slides selected")


  lapply(filtered_exp$mltplx_objects,\(slide) {
    plot_intensity_surface(slide, types = types)
  })
}

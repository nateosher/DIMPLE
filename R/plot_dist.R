#' Plots distance matrices for selected slides in a `MltplxExperiment` object
#' @param mltplx_experiment `MltplxExperiment` objects
#' @param slide_ids Vector of ids of slides whose distance matrices you'd like
#' to print
#' @param mode String indicating plot type, either "heatmap" or "network"
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_dist.MltplxExperiment <- function(mltplx_experiment, slide_ids, mode = "heatmap") {
    filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
    for(mltplx_object in filtered_exp) {
      plot_dist(mltplx_object, mode = mode)
    }
}

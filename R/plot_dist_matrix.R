#' Plots distance matrices for selected slides in a `MltplxExperiment` object
#' @param mltplx_experiment `MltplxExperiment` objects
#' @param slide_ids Vector of ids of slides whose distance matrices you'd like
#' to print
#' @param mode String indicating plot type, either "heatmap" or "network"
#' @param net_threshold When mode is "network", edges below this absolute value are
#' excluded from the plot
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_dist_matrix.MltplxExperiment <- function(mltplx_experiment, slide_ids, mode = "heatmap",
                                       net_threshold = 0, invert_dist = TRUE,...) {
    experiment_ids = map_chr(mltplx_experiment$mltplx_objects, ~ .x$slide_id)
    if(!any(slide_ids %in% experiment_ids))
      stop("no slide ids passed as argument are present in `MlptlxExperiment` object")

    filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
    for(mltplx_object in filtered_exp) {
      plot_dist_matrix(mltplx_object, mode = mode, net_threshold = net_threshold,
                invert_dist = invert_dist,...)
    }
}


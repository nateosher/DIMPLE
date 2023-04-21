#' Plots quantile distance matrices for selected slides in a `MltplxExperiment` object
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
plot_qdist_matrix.MltplxExperiment <- function(mltplx_experiment, slide_ids,
                                               mode = "heatmap",
                                               net_threshold = 0) {
  stopifnot("Quantile distances must exist"=!is.null(mltplx_experiment$mltplx_objects[[1]]$quantile_dist))
  filtered_exp = filter_MltplxExp(mltplx_experiment, slide_ids)
  for(i in 1:length(filtered_exp$mltplx_objects)){
    print(plot_qdist_matrix(filtered_exp[[i]], mode = mode,
                            net_threshold = net_threshold))
  }
}

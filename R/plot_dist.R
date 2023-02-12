#' Plots distance matrices for selected slides in a `MltplxExperiment` object
#' @param mltplx_experiment `MltplxExperiment` objects
#' @param slide_ids Vector of ids of slides whose distance matrices you'd like
#' to print
#' @param mode String indicating plot type; currently only "heatmap" is
#' supported
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap") {
  if(mode == "heatmap") {
    mltplx_experiment %>%
      dist_to_df() %>%
      tidyr::drop_na(dist) -> df

    for(id in slide_ids) {
      df %>%
        dplyr::filter(slide_id == id) %>%
        ggplot(aes(type1,type2,fill=dist)) +
        geom_tile() +
        anglex() +
        scale_fill_gradient2() +
        ggtitle(paste0("Distance matrix for slide id ", id)) -> p
      print(p)
    }
  } else if(mode == "network") {
    stop("Mode network currently unsupported")
  } else {
    stop("Mode must be either heatmap or network")
  }
}

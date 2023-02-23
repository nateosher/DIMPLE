#' Plots distance matrices for selected slides in a `MltplxExperiment` object
#' @param mltplx_experiment `MltplxExperiment` objects
#' @param slide_ids Vector of ids of slides whose distance matrices you'd like
#' to print
#' @param mode String indicating plot type, either "heatmap" or "network"
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap") {
  if(mode == "heatmap") {
    df <- mltplx_experiment %>%
      dist_to_df() %>%
      tidyr::drop_na(dist)

    for(id in slide_ids) {
      p <- df %>%
        dplyr::filter(slide_id == id) %>%
        ggplot(aes(type1,type2,fill=dist)) +
        geom_tile() +
        anglex() +
        scale_fill_gradient2() +
        ggtitle(paste0("Distance matrix for slide id ", id))
      print(p)
    }
  } else if(mode == "network") {
    filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
    for(mltplx_object in filtered_exp) {
      nms <- colnames(mltplx_object$mltplx_dist$dist)
      qgraph::qgraph(mltplx_object$mltplx_dist$dist,layout = "circle",threshold=0.1,labels=nms,label.cex=2.5,label.scale.equal=T)
    }
  } else {
    stop("Mode must be either heatmap or network")
  }
}

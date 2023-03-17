#' Plots quantile distance matrices for selected slides in a `MltplxExperiment` object
#' @param mltplx_experiment `MltplxExperiment` objects
#' @param slide_ids Vector of ids of slides whose distance matrices you'd like
#' to print
#' @param mode String indicating plot type, either "heatmap" or "network"
#' @param threshold When mode is "network", edges below this absolute value are
#' excluded from the plot
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @import qgraph
#' @importFrom magic adiag
#' @export
plot_qdist <- function(mltplx_experiment, slide_ids, mode = "heatmap", threshold = 0.1) {
  stopifnot("Quantile distances must exist"=!is.null(mltplx_experiment$mltplx_objects[[1]]$quantile_dist))
  if(mode == "heatmap") {

    df <- qdist_to_df(mltplx_experiment) %>%
      tidyr::drop_na(qdist)

    for(id in slide_ids) {
      p <- df %>%
        dplyr::filter(slide_id == id) %>%
        ggplot(aes(type1,type2,fill=qdist)) +
        geom_tile() +
        anglex() +
        viridis::scale_fill_viridis() + facet_wrap(interval~.)+
        ggtitle(paste0("Distance matrix for slide id ", id))
      print(p)
    }
  } else if(mode == "network") {
    filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
    for(mltplx_object in filtered_exp) {
      for(i in 1:length(mltplx_object$quantile_dist$quantiles$q_fac))
        arr <- mltplx_object$quantile_dist$quantile_dist_array
      block.diag <- do.call("adiag", lapply(seq(dim(arr)[3]), function(x) arr[ , , x]))
      intervals <- qdist_to_df(mltplx_object) %>%
        distinct(type1,interval)
      nms <- colnames(mltplx_object$mltplx_dist$dist)
      qgraph::qgraph(block.diag,threshold=threshold,layout="groups",
                     groups=as.factor(intervals$interval),
                     title=paste0("Network for slide id ", mltplx_object$slide_id),
                     palette="gray")

    }
  } else {
    stop("Mode must be either heatmap or network")
  }
}

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
plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap",net_threshold = 0.1) {
  experiment_ids = map_chr(mltplx_experiment$mltplx_objects, ~ .x$slide_id)
  if(!any(slide_ids %in% experiment_ids))
    stop("no slide ids passed as argument are present in `MlptlxExperiment` object")

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
        viridis::scale_fill_viridis() +
        ggtitle(paste0("Distance matrix for slide id ", id))
      print(p)
    }
  } else if(mode == "network") {
    filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
    for(mltplx_object in filtered_exp) {
      g <- igraph::graph_from_adjacency_matrix(mltplx_object$mltplx_dist$dist,weighted=TRUE)
      p <- g %>%
        ggnetwork::ggnetwork(.,layout = igraph::layout_in_circle(.)) %>%
        dplyr::mutate(sgn = factor(ifelse(sign(weight) < 0,"Negative","Positive"))) %>%
        dplyr::filter(abs(weight) >= net_threshold) %>%
        ggplot2::ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
        ggnetwork::geom_edges(aes(color=sgn,linewidth=abs(weight))) +
        ggnetwork::geom_nodes(color="#E58601",size=10) +
        ggnetwork::geom_nodetext(aes( label = name),
                                 fontface = "bold",color="#D3DDDC") +
        theme(legend.position = "none") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.background = element_rect(fill = "#24281A"),
              panel.grid = element_blank()) +
        guides(linewidth="none",color="none") +
        ggplot2::scale_color_manual(values=c("Positive"="#46ACC8","Negative"="#B40F20"))
      suppressWarnings(print(p))
    }
  } else {
    stop("Mode must be either heatmap or network")
  }
}

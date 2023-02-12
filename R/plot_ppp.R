#' Plots the point processes associated with selected slides of  a
#' `MltplxExperiment` object.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param slide_ids Vector of slide ids whose point processes you would like
#' to plot.
#' @return NULL
#' @import ggplot2
#' @importFrom magrittr `%>%`
#' @export
plot_ppp <- function(mltplx_experiment,slide_ids) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
  pats <- lapply(objs,\(obj) obj$mltplx_image$ppp)

  df <- purrr::map_df(1:length(pats),\(i) {
    pat <- pats[[i]]
    slide_id <- slide_ids[[i]]
    tibble::tibble(X=pat$x,Y=pat$y,type=pat$marks,slide_id=slide_id)
  })

  for(id in slide_ids) {
    df %>%
      dplyr::filter(slide_id == id) %>%
      ggplot(aes(X,Y,color=type,shape=type)) +
      geom_point() +
      scale_shape_manual(name = "type",
                         label = levels(df$type),
                         values=1:nlevels(df$type),drop=FALSE) +
      scale_color_manual(name = "type",
                         label = levels(df$type),
                         values=as.vector(pals::polychrome()),drop=FALSE) +
      ggtitle(paste0("Point pattern plot for slide id ", id))-> p
    print(p)
  }
}

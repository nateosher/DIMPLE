#' Plots the point processes associated with selected slides of  a
#' `MltplxExperiment` object.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param slide_ids Vector of slide ids whose point processes you would like
#' to plot.
#' @return list of ggplot2 objects
#' @import ggplot2
#' @importFrom magrittr `%>%`
#' @export
plot_ppp <- function(mltplx_experiment,slide_ids) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
  pats <- lapply(objs,\(obj) obj$mltplx_image$ppp)

  if(length(objs) == 0)
    stop("ids not present in given `MltplxExperiment` object")

  df <- purrr::map_df(1:length(pats),\(i) {
    pat <- pats[[i]]
    slide_id <- slide_ids[[i]]
    tibble::tibble(X=pat$x,Y=pat$y,type=pat$marks,slide_id=slide_id)
  })

  lapply(slide_ids,\(id) {
    df %>%
      dplyr::filter(slide_id == id) %>%
      ggplot(aes(X,Y,color=type,shape=type)) +
      geom_point(size=2) +
      scale_shape_manual(name = "type",
                         label = levels(df$type),
                         values=1:nlevels(df$type),drop=FALSE) +
      scale_color_manual(name = "type",
                         label = levels(df$type),
                         values=rep_len(cbfp, length(unique(df$type))+1),drop=FALSE) +
      ggtitle(paste0("Point pattern plot for slide id ", id))-> p
    p
  })
}

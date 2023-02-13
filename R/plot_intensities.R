#' Plots intensities for selected cell types and selected slides of
#' `MltplxExperiment` object.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param types Vector of cell types whose intensities you'd like to plot
#' @param slide_ids Vector of slide ids for which you would like to plot
#' the selected cell intensities.
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_intensities <- function(mltplx_experiment,types,slide_ids) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)

  df <- purrr::map_df(1:length(objs),\(i) {
    obj <- objs[[i]]
    intens <- obj$mltplx_intensity$intensities

    d <- intens %>%
      tibble::as_tibble() %>%
      dplyr::select(all_of(types),X,Y) %>%
      tidyr::pivot_longer(-c(X,Y),names_to = "type",values_to = "intensity")

    d$slide_id <- slide_ids[[i]]
    d
  })

  for(id in slide_ids) {
    df %>%
      dplyr::filter(slide_id == id) %>%
      ggplot(aes(X,Y,fill=intensity)) +
      geom_tile() +
      facet_wrap(~type) +
      scale_fill_gradientn(colours = terrain.colors(15)) +
      ggtitle(paste0("Intensity plot for slide id ", id)) -> p
    print(p)
  }
}

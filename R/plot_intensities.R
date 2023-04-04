#' Plots intensities for selected cell types and selected slides of
#' `MltplxExperiment` object.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param slide_ids Vector of slide ids for which you would like to plot
#' the selected cell intensities.
#' @param types Vector of cell types whose intensities you'd like to plot
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_intensities <- function(mltplx_experiment,slide_ids,types) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)

  all_cell_types = map(objs, ~ .x$mltplx_image$cell_types) %>%
    unlist() %>%
    unique()

  all_slide_ids = map_chr(objs, ~ .x$slide_id)

  if(!any(slide_ids %in% all_slide_ids))
    stop("none of the slide ids passed as arguments are present in `MltplxExperiment` object")

  if(!any(types %in% all_cell_types))
    stop("none of the cell types passed as arguments are present in subset of slides selected")

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
      viridis::scale_fill_viridis() +
      ggtitle(paste0("Intensity plot for slide id ", id)) -> p
    print(p)
  }
}

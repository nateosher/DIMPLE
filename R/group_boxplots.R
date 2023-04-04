#' Box plots of distances between cell type intensities `t1` and `t2` across
#' all slides in `mltplx_experiment`, grouped by `grouping_var`.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 String representing first cell type
#' @param t2 String representing second cell type
#' @param grouping_var Optional; variable in metadata that boxplots should be
#' grouped by
#' @return
#' @export
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @importFrom dplyr filter
group_boxplots <- function(mltplx_experiment,t1,t2,grouping_var="Group") {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  stopifnot("Patient metadata must contain grouping variable"= grouping_var %in% colnames(mltplx_experiment$metadata))

  mltplx_experiment %>%
    dist_to_df %>%
    filter(type1 == t1,
           type2 == t2) %>%
    ggplot(aes(!!sym(grouping_var),dist)) +
    geom_boxplot() +
    geom_jitter(color=cbfp[1]) +
    ggtitle(paste0("Distance between ", t1, " and ", t2)) +
    ylab("Distance")
}

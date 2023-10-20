#' Plot boxplots of qdist between two groups for a combination of cell types
#'
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 cell type 1
#' @param t2 cell type 2
#' @param grouping_var string, name of metadata column
#' @param agg_fun function to aggregate slide-level measurements to patient level.
#'
#' @return ggplot boxplots
#' @export
plot_qdist_boxplots <- function(mltplx_experiment,
                               t1,
                               t2,
                               grouping_var,
                               agg_fun = NULL) {
  if(is.null(mltplx_experiment$metadata))
    stop("Patient metadata must exist")
  if(is.null(mltplx_experiment$mltplx_objects[[1]]$quantile_dist))
    stop("Quantile distances must exist")
  if(!(grouping_var %in% colnames(mltplx_experiment$metadata)))
    stop("Patient metadata must contain grouping variable")
  
  mltplx_experiment %>%
    qdist_to_df(reduce_symmetric = F) %>%
    filter((type1 == t1 & type2 == t2) |
             (type1 == t2 & type2 == t1)) %>%
    {
      if(!is.null(agg_fun)) {
        group_by(.,patient_id,type1,type2,!!sym(grouping_var),interval) %>%
          summarise(qdist = agg_fun(qdist,na.rm=T))
      } else .
    } %>%
    ggplot(aes(!!sym(grouping_var),qdist)) +
    geom_boxplot() +
    geom_jitter(color=cbfp[1]) +
    facet_wrap(interval~.)
}

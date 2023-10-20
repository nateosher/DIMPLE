#' Plot boxplots of dist between two groups for a combination of cell types
#'
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 cell type 1
#' @param t2 cell type 2
#' @param grouping_var string, name of metadata column
#' @param agg_fun function to aggregate slide-level measurements to patient level.
#'
#' @return ggplot boxplots
#' @export
plot_dist_boxplots <- function(mltplx_experiment,
                              t1,
                              t2,
                              grouping_var,
                              agg_fun = NULL) {
  if(is.null(mltplx_experiment$metadata))
    stop("Patient metadata must exist")

  if(!(grouping_var %in% colnames(mltplx_experiment$metadata)))
    stop("Patient metadata must contain grouping variable")

  mltplx_experiment %>%
    dist_to_df() %>%
    filter((type1 == t1 & type2 == t2) |
             (type1 == t2 & type2 == t1)) %>%
    mutate(across(!!sym(grouping_var),factor)) %>%
    {
      if(!is.null(agg_fun)) {
        group_by(.,patient_id,type1,type2,!!sym(grouping_var)) %>%
          summarise(dist = agg_fun(dist,na.rm=T))
      } else .
    } %>%
    ggplot(aes(!!sym(grouping_var),dist)) +
    geom_boxplot() +
    geom_jitter(color=cbfp[1])
}

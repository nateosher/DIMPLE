#' Scatterplot of quantile distances between cell type intensities `t1` and `t2` across
#' all slides in `mltplx_experiment` by a continous covariate.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 First cell type
#' @param t2 Second cell type
#' @param cont_var continous variable from patient metadata
#' @param agg_fun optional function to aggregate slide-level measurements to patient level. Must have option to remove NA rows via `na.rm = T`.
#' @param smooth smoothing method to use, accepts `NULL` or a character vector, e.g. "lm", "loess"
#' @return ggplot2 plot
#' @importFrom magrittr `%>%`
#' @export

plot_qdist_scatter <- function(mltplx_experiment,t1,t2,cont_var="Age",agg_fun=NULL,smooth=NULL) {
  if(is.null(mltplx_experiment$metadata))
    stop("Patient metadata must exist")
  if(is.null(mltplx_experiment$mltplx_objects[[1]]$quantile_dist))
    stop("Quantile distances must exist")
  if(!(cont_var %in% colnames(mltplx_experiment$metadata)))
    stop("Patient metadata must contain variable")
  
  mltplx_experiment %>%
    qdist_to_df(reduce_symmetric = F) %>%
    filter((type1 == t1 & type2 == t2) |
             (type1 == t2 & type2 == t1)) %>%
    mutate(across(!!sym(cont_var),as.numeric)) %>%
    {
      if(!is.null(agg_fun)) {
        group_by(.,patient_id,type1,type2,interval) %>%
          mutate(qdist = agg_fun(qdist,na.rm=T)) 
      } else .
    } %>%
    ggplot(aes(!!sym(cont_var),qdist)) +
    {if(!is.null(smooth)) geom_smooth(method=smooth,color="black",linetype=2) } +
    geom_point(color=cbfp[1],size=2) +
    facet_wrap(interval~.)
  
}

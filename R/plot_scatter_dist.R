#' Scatterplot of distances between cell type intensities `t1` and `t2` across
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

plot_scatter_dist <- function(mltplx_experiment,t1,t2,cont_var="Age",agg_fun=NULL,smooth=NULL) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  
  mltplx_experiment %>%
    dist_to_df() %>%
    filter(type1 == t1,
           type2 == t2) %>% 
    mutate(across(!!sym(cont_var),as.numeric)) %>%
    {
      if(!is.null(agg_fun)) {
        group_by(.,patient_id,type1,type2,!!sym(cont_var)) %>%
          summarise(dist = agg_fun(dist,na.rm=T))
      } else .
    } %>%
    ggplot(aes(!!sym(cont_var),dist)) +
      {if(!is.null(smooth)) geom_smooth(method=smooth,color="black",linetype=2) } +  
      geom_point(color=cbfp[1],size=2) 
    
}



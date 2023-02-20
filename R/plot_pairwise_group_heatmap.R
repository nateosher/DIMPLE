#' Pairwise comparison between types across groups
#'
#' @param df tibble. a result of a multiplex analysis, eg, lm_dist
#'
#' @return ggplot heatmap
#' @export
plot_pairwise_group_heatmap <- function(df) {
  group_name <- unique(df$term)
  df %>% {
    ggplot(.,aes(type1,type2,fill=estimate)) +
    geom_tile() +
    anglex() +
    sig_stars() +
    scico::scale_fill_scico(palette = "vik",
                            label = function(z) replace(z, c(1, length(z)), 
                                                        c(paste0("Lesser in group ",group_name, " \u2193"),
                                                          paste0("Greater in group ", group_name, " \u2191"))),
                            breaks = round(seq(from=-max(abs(round(.$estimate,2)),na.rm=T),
                                               to=max(abs(round(.$estimate,2)),na.rm=T),
                                               length.out=5),2),
                            limits = c(-max(abs(round(.$estimate,2)),na.rm=T),max(abs(round(.$estimate,2)),na.rm=T))
    )
  }
}

#' Plot boxplots of dist between two groups for a combination of cell types
#'
#' @param mltplx_experiment 
#' @param t1 cell type 1
#' @param t2 cell type 2
#' @param group_factor string, name of metadata column
#' @param agg_fun function to aggregate slide-level measurements to patient level.
#'
#' @return ggplot boxplots
#' @export
typewise_boxplots <- function(mltplx_experiment,
                              t1,
                              t2,
                              group_factor,
                              agg_fun = NULL) {
  mltplx_experiment %>%
    dist_to_df() %>%
    filter(type1 == t1,
           type2 == t2) %>% 
    mutate(across(!!sym(group_factor),factor))%>%
    {
      if(!is.null(agg_fun)) {
        group_by(.,patient_id,type1,type2,!!sym(group_factor)) %>%
        summarise(dist = agg_fun(dist,na.rm=T))
      } else .
    } %>%
    ggplot(aes(!!sym(group_factor),dist)) +
    geom_boxplot() +
    geom_jitter(color="orange")
}

#' Pairwise comparison between types across groups
#'
#' @param df tibble. a result of a multiplex analysis, eg, lm_dist
#'
#' @return ggplot heatmap
#' @export
plot_pairwise_group_heatmap <- function(df,p_val_col = "p.adj",limits=NULL) {
  group_name <- unique(df$term)
  df %>%     
    na.omit(estimate) %>% 
    mutate(estimate=signif(estimate,2)) %>%
    arrange(type1, type2) %>%
    {
    ggplot(.,aes(type1,type2,fill=estimate)) +
    geom_tile() +
    anglex() +
    sig_stars(p_values = p_val_col) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
        if(!is.null(limits)){
          viridis::scale_fill_viridis(option="H",direction=-1,
            label = function(z) replace(z, c(1, length(z)),
                                        c(paste0("Lesser in ",group_name, " \u2193"),
                                          paste0("Greater in ", group_name, " \u2191"))),
            breaks = (seq(from=limits[1],
                          to=limits[2],
                          length.out=5)),
            limits = c(limits[1],limits[2]) )
        }else{
          viridis::scale_fill_viridis(option="H",direction=-1,
            label = function(z) replace(z, c(1, length(z)),
                                        c(paste0("Lesser in ",group_name, " \u2193"),
                                          paste0("Greater in ", group_name, " \u2191"))),
            breaks = (seq(from=-max(abs(.$estimate),na.rm=T),
                          to=max(abs(.$estimate),na.rm=T),
                          length.out=5)),
            limits = c(-max(abs(.$estimate),na.rm=T),max(abs(.$estimate),na.rm=T)) ) 
        }

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
    mutate(across(!!sym(group_factor),factor)) %>%
    {
      if(!is.null(agg_fun)) {
        group_by(.,patient_id,type1,type2,!!sym(group_factor)) %>%
        summarise(dist = agg_fun(dist,na.rm=T))
      } else .
    } %>%
    ggplot(aes(!!sym(group_factor),dist)) +
    geom_boxplot() +
    geom_jitter(color=cbfp[1])
}

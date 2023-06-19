#' Pairwise comparison between types across groups
#'
#' @param df tibble. a result of a multiplex analysis, eg, lm_dist
#'
#' @return ggplot heatmap
#' @export
plot_dist_regression_heatmap <- function(df,p_val_col = "p.adj",limits=NULL) {
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
      xlab("") + ylab("") +
      guides(fill = guide_colorbar(title = "Distance")) +
      theme(
        axis.text.x = element_text(face="bold", colour = "black"),
        axis.text.y = element_text(face="bold", colour = "black")
      ) +
        if(!is.null(limits)){
          scale_fill_gradient2(
            label = function(z) replace(z, c(1, length(z)),
                                        c(paste0("Lesser in ",group_name),
                                          paste0("Greater in ", group_name))),
            breaks = (seq(from=limits[1],
                          to=limits[2],
                          length.out=5)),
            limits = c(limits[1],limits[2]) )
        }else{
          scale_fill_gradient2(
            label = function(z) replace(z, c(1, length(z)),
                                        c(paste0("Lesser in ",group_name),
                                          paste0("Greater in ", group_name))),
            breaks = (seq(from=-max(abs(.$estimate),na.rm=T),
                          to=max(abs(.$estimate),na.rm=T),
                          length.out=5)),
            limits = c(-max(abs(.$estimate),na.rm=T),max(abs(.$estimate),na.rm=T)) )
        }

  }
}

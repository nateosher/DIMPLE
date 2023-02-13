#' Pairwise comparison between types across groups
#'
#' @param df tibble. a result of a multiplex analysis, eg, lm_dist
#'
#' @return ggplot heatmap
#' @export
plot_typewise_comparison <- function(df) {
  df %>% {
    ggplot(.,aes(type1,type2,fill=estimate)) +
    geom_tile() +
    anglex() +
    sig_stars() +
    scico::scale_fill_scico(palette = "vik",
                            label = function(z) replace(z, c(1, length(z)), c("Greater in group 2 \u2193", "Greater in group 1 \u2191")),
                            breaks = round(seq(from=-max(abs(round(.$estimate,2))),
                                               to=max(abs(round(.$estimate,2))),
                                               length.out=5),2),
                            limits = c(-max(abs(round(.$estimate,2))),max(abs(round(.$estimate,2))))
    )
  }
}
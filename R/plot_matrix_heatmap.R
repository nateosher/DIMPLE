#' A custom heatmap function designed to plot matrices as heatmaps.
#' @param m Matrix to be plotted
#' @param t Optional title of the resulting plot
#' @param min.v Minimum value on color legend
#' @param max.v Maximum value on color legend
#' @param show_legend If TRUE, color legend is shown; if FALSE, it is not.
#' @param reflect Whether or not to reflect matrix horizontally. If you're not
#' sure why you'd want to do this, just leave it as FALSE
#' @return `ggplot` object
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom stats reorder
plot_matrix_heatmap = function (m, t = "", min.v = NA, max.v = NA,
                          show_legend = TRUE, reflect = FALSE){
  plot.tib = tibble(r = numeric(0), c = numeric(0), val = numeric(0))
  if(reflect){
    for (i in 1:ncol(m)) {
      plot.tib = bind_rows(plot.tib, tibble(c = rep(i, nrow(m)),
                                            r = nrow(m):1,
                                            val = as.numeric(m[,i])))
    }
  }else{
    for (i in 1:ncol(m)) {
      plot.tib = bind_rows(plot.tib, tibble(c = rep(i, nrow(m)),
                                            r = 1:nrow(m),
                                            val = as.numeric(m[,i])))
    }
  }

  plot.tib$c = as.factor(plot.tib$c)
  if (is.na(min.v) || is.na(max.v)) {
    min.v = min(plot.tib$val, na.rm = T)
    max.v = max(plot.tib$val, na.rm = T)
  }

  ggplot(plot.tib) + geom_tile(mapping = aes(x = c, y = reorder(r, -r),
                                             fill = val)) +
    labs(fill = "") +
    ggtitle(t) +
    viridis::scale_fill_viridis(limits = c(min.v, max.v)) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(linetype = "solid", fill = NA),
          legend.position = ifelse(show_legend, "right", "none"))
}

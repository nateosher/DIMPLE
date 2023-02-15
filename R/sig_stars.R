#' GGplot helper to color heatmap tiles by whether or not they are significant according to an adjusted p-value
#'
#' @param alpha Significance level
#'
#' @return
#' @export
sig_stars <- function(alpha=0.05,p_values="p.adj") {
  list(geom_point(aes(shape=ifelse(!!sym(p_values) < alpha, "dot", "no_dot"))),
       scale_shape_manual(values=c(dot=8, no_dot=NA), guide="none"))
}
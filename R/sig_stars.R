#' GGplot helper to color heatmap tiles by whether or not they are significant according to an adjusted p-value
#'
#' @param alpha Significance level
#'
#' @return
#' @export
sig_stars <- function(alpha=0.05,p_values="p.adj") {
  list(
      geom_point(aes(shape=ifelse(!!sym(p_values) < alpha, "dot", "no_dot"),
                     alpha=ifelse(!!sym(p_values) < alpha, "dot", "no_dot")),
                 color='red',size=2),
      scale_shape_manual(values=c(dot=8, no_dot=0), guide="none"),
      # Rather than setting the shape to be NA, which throws a warning, I
      # set the alpha based on significance to make non-significant stars
      # invisible
      scale_alpha_manual(values=c(dot=1, no_dot=0), guide="none")
  )
}

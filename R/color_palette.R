#' set consistent ggplot2 theme and colorblind friendly palette
#'
#'
#' @export
cbfp = c("#56B4E9", "#E69F00", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


theme_set(
  theme_bw() +
    theme(legend.position = "right",legend.direction = "vertical", legend.box = "horizontal")
)

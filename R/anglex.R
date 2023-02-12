#' GGplot helper to angle x-axis labels 45 degrees
#'
#' @return
#'
#' @examples
anglex <- function() {
  list(theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1)))
}

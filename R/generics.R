#' @export
dist_to_df <- function(x,...) UseMethod("dist_to_df")

#' @export
qdist_to_df <- function(x,...) UseMethod("qdist_to_df")

#'@export
add_QuantileDist <- function(x,...) UseMethod("add_QuantileDist")

#'@export
cell_type_counts <- function(x,...) UseMethod("cell_type_counts")

#'@export
plot_dist <- function(x,...) UseMethod("plot_dist")

#'@export
as_MltplxExperiment <- function(x,...) UseMethod("as_MltplxExperiment")

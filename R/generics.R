#' @export
dist_to_df <- function(x,...) UseMethod("dist_to_df")

#' @export
qdist_to_df <- function(x,...) UseMethod("qdist_to_df")

#'@export
add_QuantileDist <- function(x,...) UseMethod("add_QuantileDist")

#'@export
cell_type_counts <- function(x,...) UseMethod("cell_type_counts")

#'@export
plot_dist_matrix <- function(x,...) UseMethod("plot_dist_matrix")

#'@export
as_MltplxExperiment <- function(x,...) UseMethod("as_MltplxExperiment")

#'@export
plot_qdist_matrix <- function(x,...) UseMethod("plot_qdist_matrix")

#'@export
plot_intensity_surface <- function(x,...) UseMethod("plot_intensity_surface")

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_dist_scatter.R
\name{plot_dist_scatter}
\alias{plot_dist_scatter}
\title{Scatterplot of distances between cell type intensities \code{t1} and \code{t2} across
all slides in \code{mltplx_experiment} by a continous covariate.}
\usage{
plot_dist_scatter(
  mltplx_experiment,
  t1,
  t2,
  cont_var = "Age",
  agg_fun = NULL,
  smooth = NULL
)
}
\arguments{
\item{mltplx_experiment}{\code{MltplxExperiment} object}

\item{t1}{First cell type}

\item{t2}{Second cell type}

\item{cont_var}{continous variable from patient metadata}

\item{agg_fun}{optional function to aggregate slide-level measurements to patient level. Must have option to remove NA rows via \code{na.rm = T}.}

\item{smooth}{smoothing method to use, accepts \code{NULL} or a character vector, e.g. "lm", "loess"}
}
\value{
ggplot2 plot
}
\description{
Scatterplot of distances between cell type intensities \code{t1} and \code{t2} across
all slides in \code{mltplx_experiment} by a continous covariate.
}

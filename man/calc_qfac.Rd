% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QuantileDist.R
\name{calc_qfac}
\alias{calc_qfac}
\title{Calculate quantile levels for each pixel, given a cell type}
\usage{
calc_qfac(q_probs, intensities, mask_type)
}
\arguments{
\item{q_probs}{Data frame with columns "from" and "to" with quantile ranges
as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
that these ranges \emph{can} overlap.}

\item{intensities}{dataframe of intensities at each pixel}

\item{mask_type}{Name of cell type to divide into intervals}
}
\value{
dataframe with intensities, pixel coordinates and quantile levels for each pixel.
}
\description{
Calculate quantile levels for each pixel, given a cell type
}

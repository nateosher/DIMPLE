#' Creates a new QuantileDist object from an intensity.
#' @param mltplx_intensity `MltplxIntensity` object
#' @param dist_metric Distance metric used to evaluate intensity distances
#' @param mask_type Name of cell type you want to make a QuantileDist of
#' @param q_probs Data frame with columns "from" and "to" with quantile ranges
#' as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
#' that these ranges *can* overlap.
#' @param dist_metric_name The distance metric you want to use to compute
#' distances between distributions.
new_QuantileDist <- function(mltplx_intensity,
                             dist_metric,
                             mask_type,
                             q_probs, # must be a dataframe of quantile probs with columns "from" and "to" and probs as e.g. 5 instead of 0.05
                             dist_metric_name) {
  intensities <- mltplx_intensity$intensities %>%
    as.data.frame()

  out_qfac <- calc_qfac(q_probs,intensities,mask_type)
  joined_q <- out_qfac$joined_q
  q <- out_qfac$q

  imat <- joined_q %>%
    select(-c(X,Y,q1,q2,p1,p2,q_fac))

  cell_types <- mltplx_intensity$cell_types
  p <- length(cell_types)
  K <- nrow(q_probs)
  quantile_dist_array <- array(dim = c(p,p,K),dimnames = list(cell_types,cell_types,NULL))

  for(k in 1:K) {
    idx <- joined_q$q_fac == k
    quantile_dist_array[,,k] <- MakeDistMat(imat[idx,], dist_metric)
  }

  structure(
    list(
      quantile_dist_array = quantile_dist_array,
      metric = dist_metric_name,
      cell_types = cell_types,
      mask_type = mask_type,
      quantiles = q,
      xy_qfac = joined_q %>% select(X,Y,q_fac)
    ),
    class = "QuantileDist"
  )

}

#' Calculate quantile levels for each pixel, given a cell type
#'
#' @param q_probs Data frame with columns "from" and "to" with quantile ranges
#' as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
#' that these ranges *can* overlap.
#' @param intensities dataframe of intensities at each pixel
#' @param mask_type Name of cell type to divide into intervals
#'
#' @return dataframe with intensities, pixel coordinates and quantile levels for each pixel.
# Note that some pixels may have more than one row associated with them (may be in more than one interval).
#' @export
#' @importFrom fuzzyjoin fuzzy_join
calc_qfac <- function(q_probs,intensities,mask_type) {
  mask_intensities <- intensities %>% pull(!!sym(mask_type))

  q <- q_probs %>%
    pmap_dfr(\(from,to) {
      as.vector(quantile(mask_intensities,probs=c(from,to)/100)) -> x
      x <- c(x,from,to)
      names(x) <- c("q1","q2","p1","p2")
      x
    }) %>%
    mutate(q_fac = factor(1:nrow(.)))
  q$q2[length(q$q2)]<-(q$q2[length(q$q2)]+.Machine$double.eps)

  joined_q <- intensities %>%
    fuzzyjoin::fuzzy_join(q,
                          by = setNames(c("q1","q2"),c(mask_type,mask_type)),
                          match_fun = list(`>=`, `<`))
  list(q=q,joined_q=joined_q)
}



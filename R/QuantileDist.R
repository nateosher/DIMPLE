#' Creates a new QuantileDist object from an intensity.
#' @param mltplx_intensity `MltplxIntensity` object
#' @param dist_metric Distance metric used to evaluate intensity distances
#' @param mask_type Name of cell type you want to make a QuantileDist of
#' @param q_probs Data frame with columns "from" and "to" with quantile ranges
#' as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
#' that these ranges *can* overlap.
#' @param dist_metric_name The distance metric you want to use to compute
#' distances between distributions.
#' @importFrom fuzzyjoin fuzzy_join
new_QuantileDist <- function(mltplx_intensity,
                             dist_metric,
                             mask_type,
                             q_probs, # must be a dataframe of quantile probs with columns "from" and "to" and probs as e.g. 5 instead of 0.05
                             dist_metric_name) {
  intensities <- mltplx_intensity$intensities %>%
    as.data.frame()

  mask_intensities <- intensities %>% pull(!!sym(mask_type))
  q <- q_probs %>%
    pmap_dfr(\(from,to) {
      as.vector(quantile(mask_intensities,probs=c(from,to)/100)) -> x
      x <- c(x,from,to)
      names(x) <- c("q1","q2","p1","p2")
      x
    }) %>%
    mutate(q_fac = factor(1:nrow(.)))


  intensities %>%
    fuzzyjoin::fuzzy_join(q,
                          by = setNames(c("q1","q2"),c(mask_type,mask_type)),
                          match_fun = list(`>=`, `<`)) -> joined_q

  imat <- joined_q %>%
    select(-c(X,Y,q1,q2,p1,p2,q_fac))

  cell_types <- mltplx_intensity$cell_types
  p <- length(cell_types)
  K <- nrow(q)
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
      quantiles = q
    ),
    class = "QuantileDist"
  )

}

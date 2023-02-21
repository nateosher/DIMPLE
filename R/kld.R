#' Computes Kullback–Leibler divergence between two vectors of values.
#' @param v1 Vector of probabilities- this represents the P distribution using
#' Wikipedia's notation.
#' @param v2 Vector of probabilities- this represents the Q distribution using
#' Wikipedia's notation.
#' @return KLD as double
#' @export
kld = function(v1, v2, normalize = TRUE){
  kld_checks(v1, v2)

  non_zero_both = which(v1 > 0 & v2 > 0)

  if(length(non_zero_both) == 0)
    stop(paste("vectors have no common entries with nonzero values-",
               "KLD cannot be computed"))

  v1_both = v1[non_zero_both]
  v2_both = v2[non_zero_both]

  if(normalize){
    v1_both = v1_both / sum(v1_both)
    v2_both = v2_both / sum(v2_both)
  }

  return(
    sum(
      v1_both * (log(v1_both) - log(v2_both))
    )
  )
}

kld_checks = function(v1, v2){
  if(length(v1) != length(v2))
    stop("vectors must be the same length")

  if(length(v1) <= 0)
    stop("vectors cannot be of length 0")

  if(!(typeof(v1) %in% c("double", "integer")))
    stop("`v1` is not a numeric vector")

  if(!(typeof(v2) %in% c("double", "integer")))
    stop("`v2` is not a numeric vector")

  if(any(v1 < 0 | v2 < 0))
    stop("vectors values must be >= 0")
}

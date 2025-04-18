#' Computes given two vectors, computes Jensen-Shannon distance between them.
#' Vectors should be the same length, and may contain zeroes; however, the
#' distance will only be computed on locations where both vectors are non-zero.
#' Note that JSD is symmetric, so jsd(v1, v2) = jsd(v2, v1).
#' @param v1 First vector of values
#' @param v2 Second vector of values
#' @return JSD as double
#' @export
jsd = function(v1, v2, base = 2, normalize = TRUE){
  jsd_checks(v1, v2)


  if(normalize){
    v1 = v1 / sum(v1)
    v2 = v2 / sum(v2)
  }

  M = 0.5 * (v1 + v2)

  return(sqrt(
    0.5 * (kld(v1, M, base = base, normalize = FALSE) + kld(v2, M, base = base, normalize = FALSE))
  ))
}

jsd_checks = function(v1, v2){
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

  non_zero_both = which(v1 > 0 & v2 > 0)

  if(length(non_zero_both) == 0)
    stop(paste("vectors have no common entries with nonzero values-",
               "JSD cannot be computed"))
}

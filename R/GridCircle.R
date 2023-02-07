#' Creates a matrix with an embedded "circle" (or as close to it as possible)
#' with the specified intensity. Note that if the circle is not fully contained
#' in the matrix, it will be truncated.
#' @param m Number of rows of overall grid
#' @param n Number of columns of overall grid
#' @param cx x coordinate of center of circle
#' @param cy y coordinate of center of circle
#' @param r Radius of circle
#' @param intensity Intensity of point pattern on the circle
#' @return Intensity matrix of specified dimension with "circle" in specified
#' location.
#' @export
GridCircle = function(m, n, cx, cy, r, intensity){
  intensity_mat = matrix(0, nrow = m, ncol = n)
  circle_mat = outer(1:m, 1:n, FUN = function(row, col){
    return(as.numeric(
      (row - cy)*(row - cy) + (col - cx)*(col - cx) < r*r
    ))
  })
  intensity_mat = intensity_mat + circle_mat * intensity
  intensity_mat = apply(intensity_mat, 2, rev)
  return(intensity_mat)
}

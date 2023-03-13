#' Creates a matrix with an embedded "circle" (or as close to it as possible)
#' with the specified intensity. Note that if the circle is not fully contained
#' in the matrix, it will be truncated.
#' @param m Number of rows of overall grid
#' @param n Number of columns of overall grid
#' @param cx x coordinate of center of circle
#' @param cy y coordinate of center of circle
#' @param r Radius of circle
#' @param intensity Intensity of point pattern on the rectangle; either a scalar
#' value that will be constant on the entire rectangle, or a vector of two values
#' indicating the values to be reached vertically or horizontally over the
#' rectangle. Horizontal gradients always run left to right, vertical always
#' run from top to bottom; by switching the order of the intensity vectors,
#' you can achieve both increasing and decreasing gradients in arbitrary
#' directions.
#' @param axis Either 'x'/'X' or 'y'/'Y'. If intensity is passed as min/max
#' gradient, this argument specifies which axis the gradient should occur across.
#' @return Intensity matrix of specified dimension with "circle" in specified
#' location.
#' @export
GridCircle = function(m, n, cx, cy, r, intensity, axis = NULL){
  intensity_mat = matrix(0, nrow = m, ncol = n)
  circle_mat = outer(1:m, 1:n, FUN = function(row, col){
    return(
      (row - cy)*(row - cy) + (col - cx)*(col - cx) < r*r
    )
  })
  if(length(intensity) == 1){
    intensity_mat = intensity_mat + as.numeric(circle_mat) * intensity
    intensity_mat = apply(intensity_mat, 2, rev)
  }else if(length(intensity) == 2){
    if(is.null(axis) || !(axis %in% c('x', 'X', 'y', 'Y'))){
      stop("axis of gradient must be specified as 'x' or 'y'")
    }else if(axis %in% c('x', 'X')){
      circle_indices = which(circle_mat, arr.ind = TRUE)
      min_col = min(circle_indices[,2])
      max_col = max(circle_indices[,2])
      intensity_dif = intensity[2] - intensity[1]

      intensity_vals = map_dbl(circle_indices[,2],
                      ~ intensity[1] + intensity_dif*(.x - min_col)/max_col)
      intensity_mat[circle_indices] = intensity_vals
    }else{
      circle_indices = which(circle_mat, arr.ind = TRUE)
      min_row = min(circle_indices[,1])
      max_row = max(circle_indices[,1])
      intensity_dif = intensity[2] - intensity[1]

      intensity_vals = map_dbl(circle_indices[,1],
                               ~ intensity[1] + intensity_dif*(.x - min_row)/max_row)
      intensity_mat[circle_indices] = intensity_vals
    }
  }else{
    stop("intensity must either be scalar or vector of length 2")
  }

  return(intensity_mat)
}

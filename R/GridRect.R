#' Creates a matrix with an embedded rectangle with the specified intensity.
#' Note that if the specified rectangle is not fully contained in the matrix,
#' it will be truncated.
#' @param m Number of rows of overall grid
#' @param n Number of columns of overall grid
#' @param bot_left_corner_x x coordinate of the bottom left corner of the rectangle
#' @param bot_left_corner_y y coordinate of the bottom left corner of the rectangle
#' @param width width of rectangle
#' @param height height of rectangle
#' @param intensity Intensity of point pattern on the rectangle; either a scalar
#' value that will be constant on the entire rectangle, or a vector of two values
#' indicating the minimum and maximum values to be reached vertically or horizontally
#' over the rectangle
#' @param axis Either 'x'/'X' or 'y'/'Y'. If intensity is passed as min/max
#' gradient, this argument specifies which axis the gradient should occur across.
#' @return Intensity matrix of specified dimension with "rectangle" in
#' specified location.
#' @export
GridRect = function(m, n, bot_left_corner_x, bot_left_corner_y, width,
                    height, intensity, axis = NULL){
  intensity_mat = matrix(0, nrow = m, ncol = n)
  rect_select_tib = tidyr::expand_grid(r = 1:height, c = 1:width)
  rect_select = map(1:nrow(rect_select_tib),
                    ~ c(rect_select_tib$r[.x], rect_select_tib$c[.x])
                    ) %>%
                    purrr::map(~ unlist(.x) + c(bot_left_corner_y - 1,
                                                bot_left_corner_x - 1)) %>%
                    purrr::keep(~ .x[1] <= m && .x[2] <= n)

  rect_select_rows = purrr::map_dbl(rect_select, ~ .x[1])
  rect_select_cols = purrr::map_dbl(rect_select, ~ .x[2])
  intensity_mat[rect_select_rows, rect_select_cols] = intensity
  intensity_mat = apply(intensity_mat, 2, rev)
  return(intensity_mat)
}

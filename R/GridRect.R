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
#' indicating the values to be reached vertically or horizontally over the
#' rectangle. Horizontal gradients always run left to right, vertical always
#' run from top to bottom; by switching the order of the intensity vectors,
#' you can achieve both increasing and decreasing gradients in arbitrary
#' directions.
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

  if(length(intensity) == 1){
    intensity_mat[rect_select_rows, rect_select_cols] = intensity
  }else if(length(intensity) == 2){
    if(is.null(axis) || !(axis %in% c('x', 'X', 'y', 'Y'))){
      stop("axis of gradient must be specified as 'x' or 'y'")
    }else if(axis %in% c('x', 'X')){
      intensity_gradient = seq(intensity[1], intensity[2],
                               length.out = width)
      rect_select_rows = unique(rect_select_rows)
      rect_select_cols = unique(rect_select_cols)
      for(r in rect_select_rows){
        intensity_mat[r, rect_select_cols] = intensity_gradient
      }
    }else{
      intensity_gradient = seq(intensity[1], intensity[2],
                               length.out = height)
      rect_select_rows = sort(unique(rect_select_rows), decreasing = TRUE)
      rect_select_cols = unique(rect_select_cols)
      for(c in rect_select_cols){
        intensity_mat[rect_select_rows, c] = intensity_gradient
      }
    }

  }else{
    stop("intensity must either be scalar or vector of length 2")
  }

  intensity_mat = apply(intensity_mat, 2, rev)
  return(intensity_mat)
}

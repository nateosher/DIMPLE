#' Creates new `MltplxImage` object.
#' @param x X coordinates of cells
#' @param y Y coordinates of cells
#' @param marks Cell types
#' @return `MltplxImage` object
#' @importFrom spatstat.geom ppp owin as.ppp
#' @export
new_MltplxImage = function(x, y, marks){
  ppp = ppp(x = x, y = y, marks = factor(marks),
            window = owin(c(min(x), max(x)),
                          c(min(y), max(y))))

  structure(
    list(
      ppp = ppp,
      cell_types = levels(ppp$marks)
    ),
    class = "MltplxImage"
  )
}

#' @export
print.MltplxImage = function(im, ...){
  cat("MltplxImage object with", im$ppp$n, "cells\n")
  cell_type_str = paste0(im$cell_types, collapse = ", ")
  cat(length(im$cell_types), "cell types:", cell_type_str, "\n")
}

#' @export
as.ppp.MltplxImage = function(im){
  return(im$ppp)
}

#' @export
as_tibble.MltplxImage = function(im){
  return(
    tibble(
      x = im$ppp$x,
      y = im$ppp$y,
      `Cell Type` = im$ppp$marks
    )
  )
}

#' @export
plot.MltplxImage = function(im, ...){
  ggplot(as_tibble(im)) +
    geom_point(aes(x = x, y = y, color = `Cell Type`)) +
    theme_bw()
}


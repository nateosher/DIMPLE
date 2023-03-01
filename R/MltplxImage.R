#' Creates new `MltplxImage` object.
#' @param x X coordinates of cells
#' @param y Y coordinates of cells
#' @param marks Cell types
#' @return `MltplxImage` object
#' @importFrom spatstat.geom ppp owin
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
  as_tibble(im) %>% 
    ggplot() +
    geom_point(aes(x = x, y = y, color = `Cell Type`, shape=`Cell Type`),size=2) +
    scale_shape_manual(values=1:length(unique(im$cell_types)),drop=FALSE) +
    scale_color_manual( values=rep_len(cbfp, length(unique(im$cell_types))+1),drop=FALSE) +
    ggtitle("Point pattern plot")-> p
  print(p)
}


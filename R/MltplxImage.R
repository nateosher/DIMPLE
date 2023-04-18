#' Creates new `MltplxImage` object.
#'
#' @param x X coordinates of cells
#' @param y Y coordinates of cells
#' @param xrange vector of size 2 with range of x-coordinates. If NULL, will default to c(min(x),max(x))
#' @param yrange vector of size 2 with range of y-coordinates. If NULL, will default to c(min(y),max(y))
#' @param marks Cell types
#'
#' @return `MltplxImage` object
#' @importFrom spatstat.geom ppp owin as.ppp
#' @export
new_MltplxImage = function(x, y, marks, xrange = NULL, yrange = NULL){
  if(!is.null(xrange)){
    if(length(x) > 0){
      stopifnot("X-coordinates of cells must be within xrange!"=all(
          xrange[1] <= min(x), max(x) <= xrange[2]
        )
      )
    }
  }else{
    xrange <- c(min(x),max(x))
  }

  if(!is.null(yrange)){
    if(length(y) > 0){
      stopifnot("Y-coordinates of cells must be within yrange!"=all(
          yrange[1] <= min(y), max(y) <= yrange[2]
        )
      )
    }
  }else{
    yrange <- c(min(y),max(y))
  }


  ppp = ppp(x = x, y = y, marks = factor(marks),
            window = owin(xrange,yrange))

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
  additional_args = list(...)
  if(!is.null(additional_args$id))
    title = paste0("Slide ", additional_args$id)
  else
    title = "Unnamed Slide"

  as_tibble(im) %>%
    ggplot() +
    geom_point(aes(x = x, y = y, color = `Cell Type`, shape=`Cell Type`),size=2) +
    scale_shape_manual(values=1:length(unique(im$cell_types)),drop=FALSE) +
    scale_color_manual( values=rep_len(cbfp, length(unique(im$cell_types))+1),drop=FALSE) +
    xlim(im$ppp$window$xrange) +
    ylim(im$ppp$window$yrange) +
    theme_bw() +
    ggtitle(title)-> p
  print(p)
}


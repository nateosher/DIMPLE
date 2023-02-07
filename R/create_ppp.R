#' Create point pattern with certain cell types
#'
#' @param X X coordinates of cells
#' @param Y Y coordinates of cells
#' @param cell_types factor of cell type
#' @param keep_types string vector of types to retain in ppp, or "all" if should keep all types
#' @param ranges ranges of X and Y
#'
#' @return spatstat ppp object
#' @export
create_ppp = function(X,Y,cell_types,keep_types="all",ranges=NULL) {
  cell_types = as.factor(cell_types)
  if("all" %in% keep_types) {
    keep_types = levels(cell_types)
  } else if(!is.vector(keep_types)) {
    stop("keep_types must be a vector of strings of cell types to keep!")
  }
  if(is.null(ranges)) {
    Xmin = min(X)
    Xmax = max(X)
    Ymin = min(Y)
    Ymax = max(Y)
  } else {
    Xmin = ranges[[1]][1]
    Xmax = ranges[[1]][2]
    Ymin = ranges[[2]][1]
    Ymax = ranges[[2]][2]
  }

  df = data.frame(X=X,Y=Y,cell_types=cell_types)

  filt = df %>%
    dplyr::filter(cell_types %in% keep_types) %>%
    dplyr::mutate(cell_types = as.factor(cell_types))

  pat = spatstat.geom::ppp(filt$X,filt$Y,c(Xmin,Xmax),c(Ymin,Ymax))

  spatstat.geom::marks(pat) <- filt$cell_types

  return(pat)
}

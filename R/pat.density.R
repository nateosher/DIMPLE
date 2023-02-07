#' Create kernel density from multitype point pattern
#'
#' @param pat spatstat point pattern
#' @param eps pixel size (won't be exactly this size)
#' @param sigma function to estimate bandwidth, usually something like bw.diggle
#'
#' @return dataframe in which rows are pixels and columns are cell types
#' @export
#'
#' @examples
pat.density = function(pat,eps,sigma,...) {
  split.pat = spatstat.geom::split.ppp(pat)
  dens.split = lapply(split.pat,function(pp) {
    tryCatch({
      spatstat.core::density.ppp(pp,sigma=sigma,diggle=T,eps=eps,...)
    },
    error=function(e) {
      spatstat.core::density.ppp(pp,diggle=T,eps=eps,...)
    })
  })
  sp = dens.split[[2]]
  Y = rep(sp$yrow,times=sp$dim[2])
  X = rep(sp$xcol,each=sp$dim[1])
  step = sp$xstep
  dens = lapply(dens.split,function(d) {
    c(d$v)
  }) %>%
    do.call(cbind,.) %>%
    tibble::as_tibble()

  list(dens=dens,X=X,Y=Y,step=sp$xstep,dim = c(sp$dim[1],sp$dim[2]))
}

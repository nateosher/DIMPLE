jsd_unnormalized <- function(px,py,base=exp(1)){
  if (any(px <= 0))
    px <- exp(px)
  if (any(py <= 0))
    py <- exp(py)
  px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
  py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
  pmean = 1/2*(px + py)
  KLD.px.pmean <- px * (log(px, base = base) - log(pmean, base = base))
  KLD.py.pmean <- py * (log(py, base = base) - log(pmean, base = base))
  JSD = sqrt(sum(KLD.px.pmean) + sum(KLD.py.pmean))
  return(JSD)
}

jensen_shannon_dist <- function(px,py){
  px = px/sum(px+ .Machine$double.xmin)
  py = py/sum(py+ .Machine$double.xmin)
  px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
  py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
  pmean = 1/2*(px + py)
  JSD = sqrt(LaplacesDemon::KLD(px, pmean)$sum.KLD.px.py + LaplacesDemon::KLD(py, pmean)$sum.KLD.px.py)
  return(JSD)
}

KL_div <- function(px,py){
  px = px/(sum(px) + .Machine$double.eps)
  py = py/(sum(py) + .Machine$double.eps)
  px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
  py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
  KLD.res = LaplacesDemon::KLD(px, py)$sum.KLD.px.py
  
  return(KLD.res)
}

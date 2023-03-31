reorder_list <- function(l,x,y) {
  l <- l[order(match(x,y))]
  return(l)
}
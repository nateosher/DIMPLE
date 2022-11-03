#' hello_world.R
#' This is a dummy file for illustration purposes- I'll remove
#' it for releases. But between this and the tests I've written,
#' this should offer a reasonable example.
#' @param name string- name you want to say hello to
#' @return string
#' @export

hello_world = function(name){
  return(paste0("Hello, ", name, "!"))
}

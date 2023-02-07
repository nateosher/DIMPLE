ProgressBar = function(cur, total,...){
  n_equals = min(floor((cur/total) * (options("width")$width - 20)),
                 options("width")$width - 20)
  n_space = options("width")$width - n_equals - 20
  spinny_thing = rep(c("-", "\\", "|", "/"), each = 1)[(n_equals %% 4) + 1]
  if(cur > total){
    status = "  Done   \n\n"
    spinny_thing = "+"
  }else{
    status = "  Loading"
  }

    message(c("\r[",
          rep("=", n_equals),
          rep("-", n_space),
          "]  ", spinny_thing, status), sep = ""
    , appendLF = F)
}

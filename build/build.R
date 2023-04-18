library(here)

roxygen2::roxygenize()
devtools::build_readme()
devtools::build_vignettes()
devtools::test()
# This object can be inspected for notes, errors, and warnings
dev_check = devtools::check()
dev_check
devtools::install_local(force = TRUE, build_vignettes = TRUE)

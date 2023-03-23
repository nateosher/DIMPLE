library(here)

# all_R_scripts = dir(here("R"), pattern = "*.R", recursive = TRUE,
#                     full.names = TRUE)
# file.copy(all_R_scripts, here("R"), overwrite = FALSE)

roxygen2::roxygenize()
devtools::build_readme()
devtools::build_vignettes()
devtools::test()
# This object can be inspected for notes, errors, and warnings
dev_check = devtools::check()
dev_check
devtools::install_local(force = TRUE)

exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=10)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("`plot_quantile_mask` works", {
  expect_no_error({
    plot_quantile_mask(exp, "X1", tibble(from = c(10, 30, 50, 70),
                                         to = c(20, 40, 60, 80)),
                       slide_ids = "S4")
  })
})

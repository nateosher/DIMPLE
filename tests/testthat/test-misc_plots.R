exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=10)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("`plot_qdist` works", {
  expect_error({
    plot_qdist(exp, "S5")
  }, "Quantile distances must exist")

  exp = add_QuantileDist(exp, cor, "X1",
                         tibble(from = seq(10, 50, 10), to = seq(20, 60, 10)))
  expect_no_error({
    plot_qdist(exp, "S5")
  })

  expect_no_error({
    plot_qdist(exp, "S5", mode = "network", threshold = 0)
  }) %>%
    expect_warning("Non-finite weights are omitted")

  expect_error({
    plot_qdist(exp, "S5", mode = "joel mode")
  }, "Mode must be either heatmap or network")

})

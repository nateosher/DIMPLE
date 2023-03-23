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

test_that("`patient_boxplots` works", {
  expect_no_error({
    boxplots_1 = patient_boxplots(exp, "X1", "X2", grouping_var = "group")
  })

  expect_equal(nrow(boxplots_1$data), 19)
  expect_equal(ncol(boxplots_1$data), 15)
  exp_no_meta = exp
  exp_no_meta$metadata = NULL

  expect_error({
    patient_boxplots(exp_no_meta, "X1", "X2", grouping_var = "group")
  },"Patient metadata must exist")
})

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

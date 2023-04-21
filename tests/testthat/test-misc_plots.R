exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=10)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("`plot_dist_regression_heatmap` works", {
  lm_dist_max <- lm_dist(exp, "group", agg_fun = max)

  expect_no_warning({
    plot_1 = plot_dist_regression_heatmap(lm_dist_max, p_val_col = "p.value")
  })

  expect_true(all(
    colnames(plot_1$data) ==
      c("type1", "type2", "term", "estimate", "std.error",
        "statistic", "p.value", "p.adj")
  ))

  expect_true(all(
    plot_1$data$type1 == c("X1", "X1", "X2")
  ))

  expect_equal(length(plot_1$layers), 2)
  expect_equal(rlang::quo_get_expr(plot_1$mapping$x) %>% as.character(),
               "type1")
  expect_equal(rlang::quo_get_expr(plot_1$mapping$y) %>% as.character(),
               "type2")
  expect_equal(rlang::quo_get_expr(plot_1$mapping$fill) %>% as.character(),
               "estimate")

  expect_no_warning({
    plot_1 = plot_dist_regression_heatmap(lm_dist_max, p_val_col = "p.value")
  })

  expect_true(all(
    colnames(plot_1$data) ==
      c("type1", "type2", "term", "estimate", "std.error",
        "statistic", "p.value", "p.adj")
  ))

  expect_true(all(
    plot_1$data$type1 == c("X1", "X1", "X2")
  ))

  expect_equal(length(plot_1$layers), 2)
  expect_equal(rlang::quo_get_expr(plot_1$mapping$x) %>% as.character(),
               "type1")
  expect_equal(rlang::quo_get_expr(plot_1$mapping$y) %>% as.character(),
               "type2")
  expect_equal(rlang::quo_get_expr(plot_1$mapping$fill) %>% as.character(),
               "estimate")

  # I can't figure out how to access the legend settings, but at the very least
  # this should work without errors or warnings
  expect_no_warning({
    plot_2 = plot_dist_regression_heatmap(lm_dist_max, limits = c(-0.1, 0.1),
                                         p_val_col = "p.value")
  })
})

test_that("`plot_dist_boxplots` works", {
  expect_no_error({
    plot_1 = plot_dist_boxplots(exp, "X1", "X2", grouping_var = "group")
  })

  # Slide S4 doesn't have any cells of type 2; so there should only be 19 rows
  expect_equal(nrow(plot_1$data), 19)

  # But still 7 columns
  expect_equal(ncol(plot_1$data), 7)

  expect_no_error({
    plot_2 = plot_dist_boxplots(exp, "X1", "X2", grouping_var = "group", agg_fun = max)
  })

  # Here there should be 10, since patient 4 has another slide with cells of
  # all types
  expect_equal(nrow(plot_2$data), 10)

  # But still 6 columns
  expect_equal(ncol(plot_2$data), 6)

  expect_error({
    plot_dist_boxplots(exp %>% (\(x) {
      x$metadata = NULL
      x
    }), "X1", "X2", grouping_var = "group")
  }, "Patient metadata must exist")

  expect_error({
    plot_dist_boxplots(exp, "X1", "X2", grouping_var = "nonexistant")
  }, "Patient metadata must contain grouping variable")
})

test_that("`plot_quantile_intensity_surface` works", {
  expect_no_error({
    plot_quantile_intensity_surface(exp, "X1", tibble(from = c(10, 30, 50, 70),
                                         to = c(20, 40, 60, 80)),
                       slide_ids = "S4")
  })
})

test_that("`patient_boxplots` works", {
  expect_no_error({
    boxplots_1 = patient_boxplots(exp, "X1", "X2", grouping_var = "group")
  })

  expect_equal(nrow(boxplots_1$data), 19)
  expect_equal(ncol(boxplots_1$data), 18)
  exp_no_meta = exp
  exp_no_meta$metadata = NULL

  expect_error({
    patient_boxplots(exp_no_meta, "X1", "X2", grouping_var = "group")
  },"Patient metadata must exist")
})

test_that("`plot_qdist_matrix` works", {
  expect_error({
    plot_qdist_matrix(exp, "S5")
  }, "Quantile distances must exist")

  exp = add_QuantileDist(exp, cor, "X1",
                         tibble(from = seq(10, 50, 10), to = seq(20, 60, 10)))
  expect_no_error({
    plot_qdist_matrix(exp, "S5")
  })

  expect_no_error({
    plot_qdist_matrix(exp, "S5", mode = "network", net_threshold = 0)
  })

  expect_error({
    plot_qdist_matrix(exp, "S5", mode = "joel mode")
  }, "Mode must be either heatmap or network")
})

test_that("`plot_dist_scatter` works", {
  expect_no_error({
    scatter_1 = plot_dist_scatter(exp, "X1", "X2", cont_var = "age")
  })

  expect_equal(nrow(scatter_1$data), 19)
  expect_equal(ncol(scatter_1$data), 7)
  exp_no_meta = exp
  exp_no_meta$metadata = NULL

  expect_error({
    plot_dist_scatter(exp_no_meta, "X1", "X2", cont_var = "age")
  },"Patient metadata must exist")
})

test_that("`plot_ppp` works", {
  expect_no_error({
    plot_ppp(exp, paste0("S", 1:4))
  })

  expect_error({
    plot_ppp(exp, "nonexistant")
  }, "ids not present in given `MltplxExperiment` object")
})

test_that("`plot_dist_matrix.MltplxObject` works", {
  expect_no_error({
    plot_dist_matrix(exp[[1]])
  })

  expect_no_error({
    plot_dist_matrix(exp[[1]], mode = "network")
  })

  expect_no_error({
    plot_dist_matrix(exp[[1]], mode = "network", invert_dist = FALSE)
  })

})

test_that("`plot_dist_matrix.MltplxExperiment` works", {
  expect_no_error({
    plot_dist_matrix(exp, "S1")
  })

  expect_error({
    plot_dist_matrix(exp, "nonexistant")
  }, "no slide ids passed as argument are present in `MlptlxExperiment` object")
})

test_that("`plot_intensity_surface` works", {
  expect_no_error({
    plot_intensity_surface(exp, c("S1", "S2"), "X1")
  })

  expect_error({
    plot_intensity_surface(exp, c("S1", "S2"), "nonexistant")
  }, "none of the cell types passed as arguments are present in subset of slides selected")

  expect_error({
    plot_intensity_surface(exp, "nonexistant", "X1")
  }, "none of the slide ids passed as arguments are present in `MltplxExperiment` object")

})

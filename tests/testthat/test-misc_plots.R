exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=10)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("`plot_pairwise_group_heatmap` works", {
  lm_dist_max <- lm_dist(exp, "group", agg_fun = max)

  expect_no_warning({
    plot_1 = plot_pairwise_group_heatmap(lm_dist_max, p_val_col = "p.value")
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
    plot_1 = plot_pairwise_group_heatmap(lm_dist_max, p_val_col = "p.value")
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
    plot_2 = plot_pairwise_group_heatmap(lm_dist_max, limits = c(-0.1, 0.1),
                                         p_val_col = "p.value")
  })


})

test_that("`typewise_boxplots` works", {
  expect_no_error({
    plot_1 = typewise_boxplots(exp, "X1", "X2", group_factor = "group")
  })

  # Slide S4 doesn't have any cells of type 2; so there should only be 19 rows
  expect_equal(nrow(plot_1$data), 19)

  # But still 6 columns
  expect_equal(ncol(plot_1$data), 6)

  expect_no_error({
    plot_2 = typewise_boxplots(exp, "X1", "X2", group_factor = "group", agg_fun = max)
  })

  # Here there should be 10, since patient 4 has another slide with cells of
  # all types
  expect_equal(nrow(plot_2$data), 10)

  # But still 6 columns
  expect_equal(ncol(plot_2$data), 6)
})
























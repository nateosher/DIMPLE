test_that("`plot_matrix_heatmap` works", {
  matrix_1 = GridCircle(100, 100, 30, 30, 10, 0.1) +
             GridCircle(100, 100, 70, 70, 10, 0.15)

  plot_1 = plot_matrix_heatmap(matrix_1)

  expect_true(all(
    plot_1$data %>% dim() == c(10000, 3)
  ))

  expect_true(all(
    plot_1$data$r == rep(1:100, 100)
  ))

  expect_true(all(
    plot_1$data$c == rep(1:100, each = 100)
  ))

  expect_equal(plot_1$theme$legend.position, "right")

})

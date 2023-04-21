test_that("`GridRect` works", {
  grid1 = GridRect(5, 5, 1, 1, 2, 3, 0.1)

  expected_grid_1 = matrix(c(
    c(0,   0,   0, 0, 0),
    c(0,   0,   0, 0, 0),
    c(0.1, 0.1, 0, 0, 0),
    c(0.1, 0.1, 0, 0, 0),
    c(0.1, 0.1, 0, 0, 0)
  ), nrow = 5, ncol = 5, byrow = TRUE)

  # correct dimensions
  expect_true(all(
    dim(grid1) == dim(expected_grid_1)
  ))

  # correct entries
  expect_true(all(
    grid1 == expected_grid_1
  ))

  # I currently allow the rectangle to be specified anywhere, and simply
  # truncate it if it overflows; want to check that this works

  # i.e. this should result in a matrix of all zeroes
  grid2 = GridRect(5, 5, 10, 10, 2, 3, 0.1)
  expect_true(all(
    grid2 == 0
  ))

  # And this one should be truncated to a square in the top right
  grid3 = GridRect(5, 5, 4, 4, 10, 10, 0.1)
  expected_grid_3 = matrix(c(
    c(0, 0, 0, 0.1, 0.1),
    c(0, 0, 0, 0.1, 0.1),
    c(0, 0, 0, 0,   0  ),
    c(0, 0, 0, 0,   0  ),
    c(0, 0, 0, 0,   0  )
  ), nrow = 5, ncol = 5, byrow = TRUE)

  expect_true(all(
    grid3 == expected_grid_3
  ))

  # Gradients
  grid4 = GridRect(5, 5, 1, 1, 3, 3, c(0.1, 0.3), axis = 'x')
  expected_grid_4 = matrix(c(
    c(0,   0,   0,   0, 0),
    c(0,   0,   0,   0, 0),
    c(0.1, 0.2, 0.3, 0, 0),
    c(0.1, 0.2, 0.3, 0, 0),
    c(0.1, 0.2, 0.3, 0, 0)
  ), nrow = 5, ncol = 5, byrow = TRUE)

  expect_true(all(
    grid4 == expected_grid_4
  ))

  grid5 = GridRect(5, 5, 1, 1, 3, 3, c(0.3, 0.1), axis = 'x')
  expected_grid_5 = matrix(c(
    c(0,   0,   0,   0, 0),
    c(0,   0,   0,   0, 0),
    c(0.3, 0.2, 0.1, 0, 0),
    c(0.3, 0.2, 0.1, 0, 0),
    c(0.3, 0.2, 0.1, 0, 0)
  ), nrow = 5, ncol = 5, byrow = TRUE)

  expect_true(all(
    grid5 == expected_grid_5
  ))

  grid6 = GridRect(5, 5, 1, 1, 3, 3, c(0.1, 0.3), axis = 'y')
  expected_grid_6 = matrix(c(
    c(0,   0,   0,   0, 0),
    c(0,   0,   0,   0, 0),
    c(0.1, 0.1, 0.1, 0, 0),
    c(0.2, 0.2, 0.2, 0, 0),
    c(0.3, 0.3, 0.3, 0, 0)
  ), nrow = 5, ncol = 5, byrow = TRUE)

  expect_true(all(
    grid6 == expected_grid_6
  ))

  grid7 = GridRect(5, 5, 1, 1, 3, 3, c(0.3, 0.1), axis = 'y')
  expected_grid_6 = matrix(c(
    c(0,   0,   0,   0, 0),
    c(0,   0,   0,   0, 0),
    c(0.3, 0.3, 0.3, 0, 0),
    c(0.2, 0.2, 0.2, 0, 0),
    c(0.1, 0.1, 0.1, 0, 0)
  ), nrow = 5, ncol = 5, byrow = TRUE)

  expect_true(all(
    grid7 == expected_grid_6
  ))
})

test_that("`GridCircle` works", {
  circle_1 = GridCircle(11, 11, 6, 6, 5, 0.1)
  expected_circle_1 = matrix(c(
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0, 0.0, 0),
    c(0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0, 0),
    c(0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0),
    c(0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0),
    c(0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0),
    c(0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0),
    c(0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0),
    c(0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0, 0),
    c(0, 0.0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0)
  ), nrow = 11, ncol = 11, byrow = TRUE)

  expect_true(all(
    dim(circle_1) == dim(expected_circle_1)
  ))

  expect_true(all(
    circle_1 == expected_circle_1
  ))

  # Also checking overflow for circle
  circle_2 = GridCircle(11, 11, 11, 6, 5, 0.1)
  expected_circle_2 = matrix(c(
   c(0, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0),
   c(0, 0, 0, 0, 0, 0, 0.0, 0.0, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.0, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.0, 0.1, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.0, 0.0, 0.1, 0.1, 0.1),
   c(0, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0)
  ), nrow = 11, ncol = 11, byrow = TRUE)
  expect_true(all(
    circle_2 == expected_circle_2
  ))

  # Gradients
  circle_3 = GridCircle(11, 11, 6, 6, 5, c(0.1, 0.9), axis = 'x')
  expected_circle_3 = matrix(c(
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.3, 0.4, 0.5, 0.6, 0.7, 0.0, 0.0, 0),
    c(0, 0.0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.0, 0),
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0),
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0),
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0),
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0),
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0),
    c(0, 0.0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.0, 0),
    c(0, 0.0, 0.0, 0.3, 0.4, 0.5, 0.6, 0.7, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0)
  ), nrow = 11, ncol = 11, byrow = TRUE)

  # This is because of a REALLY annoying bug
  expect_true(all(abs(
    (circle_3 - expected_circle_3) < 0.00000001
  )))

  circle_4 = GridCircle(11, 11, 6, 6, 5, c(0.9, 0.1), axis = 'x')
  expected_circle_4 = matrix(c(
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.7, 0.6, 0.5, 0.4, 0.3, 0.0, 0.0, 0),
    c(0, 0.0, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.0, 0),
    c(0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0),
    c(0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0),
    c(0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0),
    c(0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0),
    c(0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0),
    c(0, 0.0, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.0, 0),
    c(0, 0.0, 0.0, 0.7, 0.6, 0.5, 0.4, 0.3, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0)
  ), nrow = 11, ncol = 11, byrow = TRUE)

  # This is because of a REALLY annoying bug
  expect_true(all(abs(
    (circle_4 - expected_circle_4) < 0.00000001
  )))

  circle_5 = GridCircle(11, 11, 6, 6, 5, c(0.1, 0.9), axis = 'y')
  expected_circle_5 = matrix(c(
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0, 0.0, 0),
    c(0, 0.0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.0, 0),
    c(0, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0),
    c(0, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0),
    c(0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0),
    c(0, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0),
    c(0, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0),
    c(0, 0.0, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.0, 0),
    c(0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0)
  ), nrow = 11, ncol = 11, byrow = TRUE)

  # This is because of a REALLY annoying bug
  expect_true(all(abs(
    (circle_5 - expected_circle_5) < 0.00000001
  )))

  circle_6 = GridCircle(11, 11, 6, 6, 5, c(0.9, 0.1), axis = 'y')
  expected_circle_6 = matrix(c(
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.0, 0.0, 0),
    c(0, 0.0, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.0, 0),
    c(0, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0),
    c(0, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0),
    c(0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0),
    c(0, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0),
    c(0, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0),
    c(0, 0.0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.0, 0),
    c(0, 0.0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0, 0.0, 0),
    c(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0)
  ), nrow = 11, ncol = 11, byrow = TRUE)

  # This is because of a REALLY annoying bug
  expect_true(all(abs(
    (circle_6 - expected_circle_6) < 0.00000001
  )))


})

test_that("`SimulateGrid` works", {
  set.seed(24601)
  sim1 = SimulateGrid(
    list(
      GridRect(100, 100, 1,  1,  20, 20, 0.1),
      GridRect(100, 100, 80, 80, 20, 20, 0.1)
    )
  )

  # Should return a `MltplxObject`
  expect_equal(class(sim1), "MltplxObject")

  # There should be at least one point in the lower left square
  expect_true(any(sim1$mltplx_image$ppp$x < 20 & sim1$mltplx_image$ppp$y < 20))

  # And in the upper right square
  expect_true(any(sim1$mltplx_image$ppp$x > 80 & sim1$mltplx_image$ppp$y > 80))

  # The marks should be discrete
  expect_equal(sim1$mltplx_image$ppp$marks %>% class(), "factor")

  # And there should be two marks: 1 and 2
  expect_true(all(
    sim1$mltplx_image$ppp$marks %>% levels() == c("Type 1", "Type 2")
  ))

  # Handle edge case where no points are generated
  expect_no_error({
    sim2 = SimulateGrid(
      list(
        GridRect(100, 100, 1,  1,  20, 20, 0),
        GridRect(100, 100, 80, 80, 20, 20, 0)
      )
    )
  })

  expect_equal(sim2$mltplx_image$ppp$n, 0)

  expect_true(all(
    sim2$mltplx_image$ppp$window$xrange == c(0, 100)
  ))

  expect_true(all(
    sim2$mltplx_image$ppp$window$yrange == c(0, 100)
  ))
})

test_that("`plot_simulation_heatmap` works", {
  matrix_1 = GridCircle(100, 100, 30, 30, 10, 0.1) +
    GridCircle(100, 100, 70, 70, 10, 0.15)

  plot_1 = plot_simulation_heatmap(matrix_1)

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

  # You should be able to manually set the scale without issue
  expect_no_error({
    plot_2 = plot_simulation_heatmap(matrix_1, min.v = 0.11, max.v = 0.16)
  })

})


































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

})

test_that("`SimulateGrid` works", {
  set.seed(24601)
  sim1 = SimulateGrid(
    list(
      GridRect(100, 100, 1,  1,  20, 20, 0.1),
      GridRect(100, 100, 80, 80, 20, 20, 0.1)
    )
  )
  # There should be at least one point in the lower left square
  expect_true(any(sim1$x < 20 & sim1$y < 20))

  # And in the upper right square
  expect_true(any(sim1$x > 80 & sim1$y > 80))

  # The marks should be discrete
  expect_equal(sim1$marks %>% class(), "factor")

  # And there should be two marks: 1 and 2
  expect_true(all(
    sim1$marks %>% levels() == c(1, 2)
  ))
})




































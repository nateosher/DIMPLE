test_that("jsd works", {
  #######################
  # Malformed inputs

  # Negative values
  expect_error(jsd(
    c(-1, 0),
    c(-1, -1)
  ))

  # Different lengths
  expect_error(jsd(
    1:5 / 15,
    1:6 / 21
  ))

  # Zero lengths
  expect_error(jsd(
    numeric(),
    c(0.5, 0.5)
  ))

  # Non numerics
  expect_error(jsd(
    c("compute", "please"),
    c(-1, -1)
  ))

  # No common entries
  expect_error(jsd(
    c(0, 0, 0.5, 0.5),
    c(0.5, 0.5, 0, 0)
  ))

  ##########################
  # Examples
  desired_precision = 1e-8
  expect_true(
    abs(
    jsd(seq(1, 10, by = 1) / sum(1:10),
        seq(10, 1, by = -1) / sum(1:10),
        base = exp(1)) -
    0.151303372
    ) <= desired_precision
  )

  expect_true(
    abs(
      jsd(seq(1, 100, by = 1) / sum(1:100),
          seq(100, 1, by = -1) / sum(1:100),
          base = exp(1)) -
        0.188272596
    ) <= desired_precision
  )

})

test_that("kld works", {
  #######################
  # Malformed inputs

  # Negative values
  expect_error(kld(
    c(-1, 0),
    c(-1, -1)
  ))

  # Different lengths
  expect_error(kld(
    1:5 / 15,
    1:6 / 21
  ))

  # Zero lengths
  expect_error(kld(
    numeric(),
    c(0.5, 0.5)
  ))

  # Non numerics
  expect_error(kld(
    c("compute", "please"),
    c(-1, -1)
  ))

  # No common entries
  expect_error(kld(
    c(0, 0, 0.5, 0.5),
    c(0.5, 0.5, 0, 0)
  ))

  ##########################
  # Examples
  desired_precision = 1e-8
  expect_true(
    abs(
      kld(seq(1, 10, by = 1) / sum(1:10),
          seq(10, 1, by = -1) / sum(1:10)) -
        0.69122041
    ) <= desired_precision
  )

  expect_true(
    abs(
      kld(seq(1, 100, by = 1) / sum(1:100),
          seq(100, 1, by = -1) / sum(1:100)) -
        0.94570435
    ) <= desired_precision
  )

})

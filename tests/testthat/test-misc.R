test_that("`create_ppp` works", {
  set.seed(24601)
  x = runif(100, 0, 300)
  y = runif(100, 0, 300)
  marks = sample(c("Tumor", "Immune", "Stroma"), 100, replace = TRUE)

  expect_no_error({
    ppp_1 = create_ppp(x, y, marks)
  })

  ppp_2 = create_ppp(x, y, marks, keep_types = c("Tumor", "Immune"))
  expect_equal(ppp_2$n, 73)
  expect_equal(ppp_2$marks %>% levels() %>% length(), 2)
  expect_true({
    all(
      levels(ppp_2$marks) ==
        c("Immune", "Tumor")
    )
  })

  expect_warning({
    ppp_3 = create_ppp(x, y, marks, ranges = list(c(0, 150), c(0, 150)))
  }, "71 points were rejected as lying outside the specified window")

  expect_equal(ppp_3$n, 29)
})

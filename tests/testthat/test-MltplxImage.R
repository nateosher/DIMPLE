set.seed(24601)
cell_x_values = runif(3000, 0, 600)
cell_y_values = runif(3000, 0, 600)
cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
slide_ids = rep(paste("Slide", 1:10), each = 300)
slide_tibble = tibble(
  x = cell_x_values,
  y = cell_y_values,
  marks = cell_marks,
  id = slide_ids
)

test_that("`MltplxImage` constructor works", {
  image_1 = new_MltplxImage(slide_tibble %>%
                              filter(id == "Slide 1") %>%
                              pull(x),
                            slide_tibble %>%
                              filter(id == "Slide 1") %>%
                              pull(y),
                            slide_tibble %>%
                              filter(id == "Slide 1") %>%
                              pull(marks))

  expect_true(all(
    sort(image_1$cell_types) == c("Immune", "Other", "Tumor")
  ))

  expect_true(all(
    levels(image_1$ppp$marks) == c("Immune", "Other", "Tumor")
  ))



  # Print
  expect_true(all(
    (print(image_1) %>% capture.output()) ==
      c("MltplxImage object with 300 cells",
        "3 cell types: Immune, Other, Tumor ")
  ))

  # Plot
  plot_1 = plot(image_1)
  expect_true(all(
    dim(plot_1$data) ==
      c(300, 3)
  ))

  expect_equal(plot_1$labels$x, "x")
  expect_equal(plot_1$labels$y, "y")
  expect_equal(plot_1$labels$colour, "Cell Type")
})

test_that("`MltplxImage` conversion functions work", {
  image_2 = new_MltplxImage(slide_tibble %>%
                              filter(id == "Slide 2") %>%
                              pull(x),
                            slide_tibble %>%
                              filter(id == "Slide 2") %>%
                              pull(y),
                            slide_tibble %>%
                              filter(id == "Slide 2") %>%
                              pull(marks))

  expect_equal(as.ppp(image_2)$n, image_2$ppp$n)
  expect_equal(as.ppp(image_2)$window, image_2$ppp$window)
  expect_equal(as.ppp(image_2)$x, image_2$ppp$x)
  expect_equal(as.ppp(image_2)$y, image_2$ppp$y)
  expect_equal(as.ppp(image_2)$marks, image_2$ppp$marks)

  expect_no_error({
    tib = as_tibble(image_2)
  })

  expect_true(all(
    class(tib) == c("tbl_df", "tbl", "data.frame")
  ))

  expect_true(all(
    colnames(tib) == c("x", "y", "Cell Type")
  ))
})

test_that("`MltplxImage` catches malformed input", {
  expect_error({
    new_MltplxImage(slide_tibble %>%
                        filter(id == "Slide 1") %>%
                        pull(x),
                      slide_tibble %>%
                        filter(id == "Slide 1") %>%
                        pull(y),
                      slide_tibble %>%
                        filter(id == "Slide 1") %>%
                        pull(marks),
                      window = 7)
  },
  "`window` argument must be of class `owin` from `spatstat.geom`")
})

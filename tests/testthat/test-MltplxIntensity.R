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

test_that("`MltplxIntensity` constructor works", {
  image_1 = new_MltplxImage(slide_tibble %>%
                              filter(id == "Slide 1") %>%
                              pull(x),
                            slide_tibble %>%
                              filter(id == "Slide 1") %>%
                              pull(y),
                            slide_tibble %>%
                              filter(id == "Slide 1") %>%
                              pull(marks))

  intensity_1 = new_MltplxIntensity(image_1, 10, 30)

  expect_true(all(
    sort(intensity_1$cell_types) == c("Immune", "Other", "Tumor")
  ))

  expect_equal(intensity_1$ps, 10)

  expect_equal(intensity_1$bw, 30)

  expect_true(all(
    intensity_1$dim == c(60, 60)
  ))

  # Print
  expect_true(all(
    (print(intensity_1) %>% capture.output()) ==
    c("MltplxIntensity object with 3 cell types",
    "3 cell types: Immune, Other, Tumor ",
    "Pixel size: 10 ",
    "Bandwidth: 30 ",
    "Grid dimensions: 60 x 60 ")
  ))

  # Plot
  plot_1 = plot(intensity_1)
  expect_true(all(
    dim(plot_1$data) ==
      c(10800, 4)
  ))

  expect_equal(plot_1$labels$title, "Cell intensities")
  expect_equal(plot_1$labels$x, "")
  expect_equal(plot_1$labels$y, "")
  expect_equal(plot_1$labels$fill, "intensity")
})

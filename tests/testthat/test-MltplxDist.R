set.seed(24601)
library(purrr)
cell_x_values = runif(3000, 0, 600)
cell_y_values = runif(3000, 0, 600)
cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
slide_ids = rep(paste("Slide", 1:10), each = 300)
cell_tibble = tibble(
  x = cell_x_values,
  y = cell_y_values,
  marks = cell_marks,
  id = slide_ids
)

test_that("`MltplxDist` constructor works", {
  id_1 = unique(slide_ids)[1]
  image_1 = new_MltplxImage(cell_tibble %>%
                              filter(id == id_1) %>%
                              pull(x),
                            cell_tibble %>%
                              filter(id == id_1) %>%
                              pull(y),
                            cell_tibble %>%
                              filter(id == id_1) %>%
                              pull(marks))

  intensity_1 = new_MltplxIntensity(image_1, ps = 30, bw = 30)

  dist_1 = new_MltplxDist(intensity_1, jsd, .dist_metric_name = "jsd")

  expect_true(all(
    dist_1 %>% print() %>% capture.output() ==
    c("MltplxDist object with 3 cell types",
      "Distance metric: jsd ",
      "",
      "           Immune      Other      Tumor",
      "Immune         NA 0.08881169 0.08352748",
      "Other  0.08881169         NA 0.08874943",
      "Tumor  0.08352748 0.08874943         NA"
      )
  ))

  expect_equal(dist_1$metric, "jsd")
  expect_true(all(
    dist_1$cell_types ==
    c("Immune", "Other", "Tumor")
  ))
})

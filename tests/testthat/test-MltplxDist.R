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

  jsd_e = function(v1, v2){
    jsd(v1, v2, base = exp(1))
  }

  dist_1 = new_MltplxDist(intensity_1, jsd_e, .dist_metric_name = "jsd")

  expect_true(all(
    dist_1 %>% print() %>% capture.output() ==
    c("MltplxDist object with 3 cell types",
      "Distance metric: jsd ",
      "",
      "          Immune     Other     Tumor",
      "Immune        NA 0.2980129 0.2890112",
      "Other  0.2980129        NA 0.2979084",
      "Tumor  0.2890112 0.2979084        NA"
      )
  ))

  expect_equal(dist_1$metric, "jsd")
  expect_true(all(
    dist_1$cell_types ==
    c("Immune", "Other", "Tumor")
  ))
  
  dist_2 = new_MltplxDist(intensity_1, kld, .dist_metric_name = "kld", symmetric=F)
  
  expect_true(all(
    dist_2 %>% print() %>% capture.output() ==
      c("MltplxDist object with 3 cell types",
        "Distance metric: kld ",
        "",
        "          Immune     Other     Tumor",
        "Immune        NA 0.3953645 0.3692495",
        "Other  0.4109716        NA 0.3887378",
        "Tumor  0.3790915 0.4127645        NA"
      )
  ))
  
})

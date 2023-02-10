set.seed(1234567)
cell_x_values = runif(3000, 0, 600)
cell_y_values = runif(3000, 0, 600)
cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
slide_ids = rep(paste("Slide", 1:10), each = 300)

test_that("`MltplxExperiment` constructor (no intensities/dists) works", {
  experiment_only = new_MltplxExperiment(x = cell_x_values,
                                         y = cell_y_values,
                                         marks = factor(cell_marks),
                                         slide_id = slide_ids)

  expect_true(is.null(experiment_only$ps))
  expect_true(is.null(experiment_only$bw))
  expect_true(is.null(experiment_only$dist_metric_name))
  expect_true(is.null(experiment_only$metadata))

  expect_equal(length(experiment_only), 10)

  expect_equal(class(experiment_only[[1]]), "MltplxObject")

  expect_equal(experiment_only[[1]]$slide_id, "Slide 1")
  expect_equal(experiment_only[[2]]$slide_id, "Slide 2")
  expect_equal(experiment_only[[3]]$slide_id, "Slide 3")
  expect_equal(experiment_only[[4]]$slide_id, "Slide 4")
  expect_equal(experiment_only[[5]]$slide_id, "Slide 5")
  expect_equal(experiment_only[[6]]$slide_id, "Slide 6")
  expect_equal(experiment_only[[7]]$slide_id, "Slide 7")
  expect_equal(experiment_only[[8]]$slide_id, "Slide 8")
  expect_equal(experiment_only[[9]]$slide_id, "Slide 9")
  expect_equal(experiment_only[[10]]$slide_id, "Slide 10")
})

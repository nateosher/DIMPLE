set.seed(1234567)
library(purrr)
cell_x_values = runif(3000, 0, 600)
cell_y_values = runif(3000, 0, 600)
cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
slide_ids = rep(paste("Slide", 1:10), each = 300)

test_that("`MltplxExperiment` constructor (no intensities/dists) works", {
  experiment_only = new_MltplxExperiment(x = cell_x_values,
                                         y = cell_y_values,
                                         marks = factor(cell_marks),
                                         slide_id = slide_ids)
  # Print
  print_output = capture.output(print(experiment_only))
  expect_equal(print_output[1], "MltplxExperiment with 10 slides")
  expect_equal(print_output[2], "No intensities generated")
  expect_equal(print_output[3], "No distance matrices generated")
  expect_equal(print_output[4], "No attached metadata")

  # Properties
  expect_true(is.null(experiment_only$ps))
  expect_true(is.null(experiment_only$bw))
  expect_true(is.null(experiment_only$dist_metric_name))
  expect_true(is.null(experiment_only$metadata))

  # Class
  expect_equal(class(experiment_only[[1]]), "MltplxObject")

  # Ordering
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

test_that("`MltplxExperiment` constructor (intensities, no dists) works", {
  experiment_with_intensities = new_MltplxExperiment(x = cell_x_values,
                                                    y = cell_y_values,
                                                    marks = factor(cell_marks),
                                                    slide_id = slide_ids,
                                                    ps = 30, bw = 40)

  # Print
  print_output = capture.output(print(experiment_with_intensities))
  expect_equal(print_output[1], "MltplxExperiment with 10 slides")
  expect_equal(print_output[2],
               "Intensities generated with pixel size 30 and bandwidth 40 ")
  expect_equal(print_output[3], "No distance matrices generated")
  expect_equal(print_output[4], "No attached metadata")

  # Properties
  expect_equal(experiment_with_intensities$ps, 30)
  expect_equal(experiment_with_intensities$bw, 40)
  expect_true(is.null(experiment_with_intensities$dist_metric_name))
  expect_true(is.null(experiment_with_intensities$metadata))

  # MltplxObjects
  # All intensities should be 400 x 5
  expect_true(all(
    map_lgl(experiment_with_intensities$mltplx_objects, \(o){
      all(
        o$mltplx_intensity$intensities %>% dim() == c(400, 5)
      )
    })
  ))

  # All intensities should have the same column names
  expect_true(all(
    map_lgl(experiment_with_intensities$mltplx_objects, \(o){
      all(
        (o$mltplx_intensity$intensities %>% colnames() ==
           c("Immune", "Other", "Tumor", "X", "Y"))
      )
    })
  ))

  # Class
  expect_equal(class(experiment_with_intensities[[1]]), "MltplxObject")

  # Ordering
  expect_equal(experiment_with_intensities[[1]]$slide_id, "Slide 1")
  expect_equal(experiment_with_intensities[[2]]$slide_id, "Slide 2")
  expect_equal(experiment_with_intensities[[3]]$slide_id, "Slide 3")
  expect_equal(experiment_with_intensities[[4]]$slide_id, "Slide 4")
  expect_equal(experiment_with_intensities[[5]]$slide_id, "Slide 5")
  expect_equal(experiment_with_intensities[[6]]$slide_id, "Slide 6")
  expect_equal(experiment_with_intensities[[7]]$slide_id, "Slide 7")
  expect_equal(experiment_with_intensities[[8]]$slide_id, "Slide 8")
  expect_equal(experiment_with_intensities[[9]]$slide_id, "Slide 9")
  expect_equal(experiment_with_intensities[[10]]$slide_id, "Slide 10")
})

test_that("`MltplxExperiment` constructor catches malformed objects", {
  # length(x) < length(y)
  expect_error(new_MltplxExperiment(x[1:10], y, marks, slide_ids))

  # length(x) > length(y)
  expect_error(new_MltplxExperiment(x, y[1:10], marks, slide_ids))

  # length(marks) != length(x), length(y)
  expect_error(new_MltplxExperiment(x, y, marks[1:10], slide_ids))

  # length(slide_ids) != length(x), length(y), marks
  expect_error(new_MltplxExperiment(x, y, marks, slide_ids[1:10]))

  # ps but no bw
  expect_error(new_MltplxExperiment(x, y, marks, slide_ids, ps = 30))

  # bw but no ps
  expect_error(new_MltplxExperiment(x, y, marks, slide_ids, bw = 30))
})

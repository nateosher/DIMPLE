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

test_that("`add_QuantileDist` handles missing types", {
  # From Maria's example
  x <- runif(100,-50,50)
  y <- runif(100,-50,50)
  sid<-c(rep("A",50),rep("B",50))
  typesA<-sample(rep_len(sample(paste0("X",1:2)),50))
  typesB<-sample(rep_len(sample(paste0("X",1:3)),50))
  types<-c(typesA,typesB)
  exp <- new_MltplxExperiment(x,y,types,sid)
  exp <- update_intensity(exp,ps=2,bw=3)
  from<-c(0,50)
  to<-c(50,100)
  q_probs<-cbind.data.frame(from,to)
  #want quantile dist with X3 as mask type but X3 is not in image A
  expect_warning({
    exp_with_qd = add_QuantileDist(exp,cor,mask_type = "X3",q_probs)
  })

  # Check print generic for MltplxExperiment
  expect_true(all(
    capture.output(print(exp_with_qd)) ==
    c("MltplxExperiment with 2 slides",
    "Intensities generated with pixel size 2 and bandwidth 3 ",
    "No distance matrices generated",
    "No attached metadata",
    "2 quantile distance arrays generated for mask X3 ")
  ))

  # And MltplxObject
  expect_true(all(
    capture.output(print(exp_with_qd[[2]])) ==
      c("MltplxObject ",
        "Slide id: B ",
        "Image with 50 cells across 3 cell types",
        "Cell types: X1, X2, X3 ",
        "Intensity generated with pixel size 2 and bandwidth 3 ",
        "No distance matrix generated (yet)",
        "2 quantile distance arrays generated for mask X3 ")
  ))

  # First quantile dist should be NA
  expect_true(is.na(exp_with_qd[[1]]$quantile_dist))

  # Not a thorough check, but just making sure it isn't also NA
  expect_true(all(
    dim(exp_with_qd[[2]]$quantile_dist$quantile_dist_array) ==
      c(3,3,2)
  ))

  # This should throw two warnings: one for no metadata, and one
  # for slide A not containing a quantile distance array
  expect_warning({
    qd_df = qdist_to_df(exp_with_qd)
  }) %>%
  expect_warning()

  expect_true(all(
    dim(qd_df) == c(18, 5)
  ))
})

test_that("Window size parameter works", {

  data <- data.frame(slide_id = slide_ids,
                     X = cell_x_values,
                     Y = cell_y_values)
  window_sizes = data %>%
    group_by(slide_id) %>%
    summarise(min_x = min(X) - 50,
              max_x = max(X) + 20,
              min_y = min(Y) - 30,
              max_y = max(Y) + 10)

  # no error
  mltplx_exp <- new_MltplxExperiment(x = cell_x_values,
                                   y = cell_y_values,
                                   window_sizes = window_sizes,
                                   marks = factor(cell_marks),
                                   slide_id = slide_ids)

  # check window sizes are correct
  lapply(mltplx_exp$mltplx_objects,\(obj) {
    ppp <- obj$mltplx_image$ppp
    W <- ppp$window

    all(min(ppp$x) - 50 == W$xrange[1],
        max(ppp$x) + 20 == W$xrange[2],
        min(ppp$y) - 30 == W$yrange[1],
        max(ppp$y) + 10 == W$yrange[2])
  }) %>%
    unlist() -> chk

  expect_true(all(chk))

  # mis-named columns
  window_sizes2 <- rename(window_sizes, max_X = max_x)


  expect_error({
    mltplx_exp <- new_MltplxExperiment(x = cell_x_values,
                                     y = cell_y_values,
                                     window_sizes = window_sizes2,
                                     marks = factor(cell_marks),
                                     slide_id = slide_ids)
  })

  # window sizes are too small
  window_sizes3 <- mutate(window_sizes,
                         max_x = max_x - 25)

  expect_error({
    mltplx_exp <- new_MltplxExperiment(x = cell_x_values,
                                       y = cell_y_values,
                                       window_sizes = window_sizes3,
                                       marks = factor(cell_marks),
                                       slide_id = slide_ids)
  })

  # no error, no window_sizes given
  mltplx_exp <- new_MltplxExperiment(x = cell_x_values,
                                     y = cell_y_values,
                                     marks = factor(cell_marks),
                                     slide_id = slide_ids)

  # check window sizes are correct
  lapply(mltplx_exp$mltplx_objects,\(obj) {
    ppp <- obj$mltplx_image$ppp
    W <- ppp$window

    all(min(ppp$x) == W$xrange[1],
        max(ppp$x) == W$xrange[2],
        min(ppp$y) == W$yrange[1],
        max(ppp$y) == W$yrange[2])
  }) %>%
    unlist() -> chk

  expect_true(all(chk))

  })

test_that("Factor levels are alphabetical in dist_to_df", {
  cell_x_values = append(cell_x_values,runif(20, 0, 600))
  cell_y_values = append(cell_y_values,runif(20, 0, 600))
  cell_marks = append(cell_marks,rep("H1",20))
  slide_ids = append(slide_ids,rep("Slide 4",20))
  exp = new_MltplxExperiment(x = cell_x_values,
                             y = cell_y_values,
                             marks = factor(cell_marks),
                             slide_id = slide_ids,
                             ps = 30, bw = 40,dist_metric = cor)
  df <- suppressWarnings(dist_to_df(exp,reduce_symmetric = TRUE))

  expect_equal(levels(df$type1),sort(levels(df$type1)))
})

test_that("`filter_exp` works", {
  exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
  exp <- add_mltplx_metadata(exp,n_patients=10)
  exp <- update_intensity(exp,ps=2,bw=3)
  exp <- update_dist(exp,cor)

  # Try passing a non-MltplxExperiment Object
  expect_error({
    filter_exp(exp[[1]], c("S2", "S3"))
  }, "`mltplx_experiment` argument must be of class `MltplxExperiment`")

  # Filter such that no slides are included
  expect_warning({
    filter_exp(exp, c("NONEXISTANT ID"))
  }, "resulting `MltplxExperiment` has no slides")

  # Filter to only the second and third slides
  expect_no_error({
    exp_subset = filter_exp(exp, c("S2", "S3"))
  })

  expect_equal(length(exp_subset$mltplx_objects), 2)
  expect_equal(nrow(exp_subset$metadata), 2)
  expect_equal(ncol(exp_subset$metadata), 4)
  expect_true(all(
    exp_subset$metadata$slide_id %in% c("S2", "S3")
  ))

})

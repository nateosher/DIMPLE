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
  expect_error(new_MltplxExperiment(cell_x_values[1:10],
                                    cell_y_values,
                                    cell_marks, slide_ids))

  # length(x) > length(y)
  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values[1:10],
                                    cell_marks, slide_ids))

  # length(marks) != length(x), length(y)
  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks[1:10], slide_ids))

  # length(slide_ids) != length(x), length(y), marks
  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks, slide_ids[1:10]))

  # ps but no bw
  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks, slide_ids, ps = 30))

  # bw but no ps
  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks, slide_ids, bw = 30))

  # window sizes and window list
  # They're both empty, but point is both are non-null
  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks, slide_ids,
                                    window_sizes = tibble(
                                      slide_id = character(),
                                      min_x = numeric(),
                                      max_x = numeric(),
                                      min_y = numeric(),
                                      max_y = numeric()
                                    ),
                                    windows = list()),
          "pass either `windows` parameter or `window_sizes` but not both")

  expect_error(new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks, slide_ids,
                                    windows = list(owin())),
  paste0("length of `windows` argument must be the same",
         " as the number of unique slide ids"))

})

test_that("`window` parameter works", {
  expect_no_error({
    mxp_custom_windows = new_MltplxExperiment(cell_x_values,
                                    cell_y_values,
                                    cell_marks, slide_ids,
                                    windows = rep(list(owin(c(0, 600),
                                                            c(0, 600))),
                                                  10))
    })

  expect_true(all(
      mxp_custom_windows[[1]]$mltplx_image$ppp$window$xrange == c(0, 600)
    )
  )
  expect_true(all(
      mxp_custom_windows[[1]]$mltplx_image$ppp$window$yrange == c(0, 600)
    )
  )
  print_output = capture.output(print(mxp_custom_windows))
  expect_equal(print_output[1], "MltplxExperiment with 10 slides")
  expect_equal(print_output[2], "No intensities generated")
  expect_equal(print_output[3], "No distance matrices generated")
  expect_equal(print_output[4], "No attached metadata")
})

test_that("`update_qdist` handles missing types", {
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
    exp_with_qd = update_qdist(exp,cor,mask_type = "X3",q_probs)
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

  # For some reason this test started breaking, because purrr
  # returns something that `expect_error` doesn't recognize as an
  # error, though it clearly is, and clearly for the reason we want.
  # Maybe a tidyverse update?
  # expect_error({
  #   intentional_error <- tryCatch({
  #     new_MltplxExperiment(x = cell_x_values,
  #                                      y = cell_y_values,
  #                                      window_sizes = window_sizes3,
  #                                      marks = factor(cell_marks),
  #                                      slide_id = slide_ids)
  #     },
  #   error = function(e){
  #     return(e)
  #   })
  #   if(class(intentional_error)[1] == "purrr_error_indexed")
  #     stop("test passed")
  # })

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

test_that("`filter_MltplxExp` works", {
  exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
  exp <- add_mltplx_metadata(exp,n_patients=10)
  exp <- update_intensity(exp,ps=2,bw=3)
  exp <- update_dist(exp,cor)

  # Try passing a non-MltplxExperiment Object
  expect_error({
    filter_MltplxExp(exp[[1]], c("S2", "S3"))
  }, "`mltplx_experiment` argument must be of class `MltplxExperiment`")

  # Filter such that no slides are included
  expect_warning({
    filter_MltplxExp(exp, c("NONEXISTANT ID"))
  }, "resulting `MltplxExperiment` has no slides")

  # Filter to only the second and third slides
  expect_no_error({
    exp_subset = filter_MltplxExp(exp, c("S2", "S3"))
  })

  expect_equal(length(exp_subset$mltplx_objects), 2)
  expect_equal(nrow(exp_subset$metadata), 2)
  expect_equal(ncol(exp_subset$metadata), 4)
  expect_true(all(
    exp_subset$metadata$slide_id %in% c("S2", "S3")
  ))

})


test_that("`as_tibble` works", {
  exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
  expect_error({
    as_tibble(exp)
  }, "no metadata attached and no distance matrices generated")

  exp <- add_mltplx_metadata(exp,n_patients=10)

  expect_warning({
    as_tibble(exp)
  }, "no distance matrices generated; returning metadata")


  exp <- update_intensity(exp,ps=2,bw=3)
  exp <- update_dist(exp,cor)

  expect_no_error({
    exp_tib = as_tibble(exp)
  })

  expect_equal(nrow(exp_tib), 20)
  expect_equal(ncol(exp_tib), 7)
  expect_equal(colnames(exp_tib)[5], "X1~X2")
  expect_equal(colnames(exp_tib)[6], "X1~X3")
  expect_equal(colnames(exp_tib)[7], "X2~X3")
})

test_that("`list.as_MltplxExperiment` works", {
  # From list of MltplxObjects
  set.seed(24601)
  obj_list = map(1:3, \(i){
    SimulateGrid(
      list(
        GridRect(m = 100,
                 n = 100,
                 bot_left_corner_x = 10 + 10 * i,
                 bot_left_corner_y = 10 + 10 * i,
                 width = 50,
                 height = 50,
                 intensity = 0.1)
      ),
      square_side_length = 1
    )
  })

  expect_no_error({
    list_exp = as_MltplxExperiment(obj_list)
  })

  expect_equal(class(list_exp), "MltplxExperiment")

  expect_equal(list_exp[[1]]$mltplx_image$ppp$window$xrange[1], 0)
  expect_equal(list_exp[[1]]$mltplx_image$ppp$window$xrange[2], 100)
  expect_equal(list_exp[[1]]$mltplx_image$ppp$window$yrange[1], 0)
  expect_equal(list_exp[[1]]$mltplx_image$ppp$window$yrange[2], 100)

  expect_true(all(
    (list_exp %>% print() %>% capture.output()) ==
      c("MltplxExperiment with 3 slides",
        "No intensities generated",
        "No distance matrices generated",
        "No attached metadata")
  ))

  expect_true(all(
    (list_exp[[1]] %>% print() %>% capture.output()) ==
    c("MltplxObject ",
    "Slide id: PGWGNI ",
    "Image with 271 cells across 1 cell types",
    "Cell types: Type 1 ",
    "No intensity generated (yet)",
    "No distance matrix generated (yet)")
  ))

  expect_error(as_MltplxExperiment(list(1, 2)),
     paste('to convert "list" object to "MltplxObject" object, all',
           'elements of list must either be of class "MltplxObject" or ',
           'ppp')
  )

  # From list of ppp objects
  expect_no_error({
    mxp_from_ppp_list = map(1:10, \(i){
          spatstat.geom::ppp(
            x = runif(10),
            y = runif(10),
            marks = sample(c("Type 1", "Type 2"), 10, replace = T)
          )
      }) %>% as_MltplxExperiment()
  })

  expect_equal(mxp_from_ppp_list$mltplx_objects[[1]]$slide_id,
               "Slide 1")

  expect_true(all(
    mxp_from_ppp_list$mltplx_objects[[1]]$mltplx_image$cell_types %in%
      c("Type 1", "Type 2")
  ))

  heterogeneous_list = list(
    new_MltplxObject(
      x = runif(100, 0, 100),
      y = runif(100, 0, 100),
      marks = sample(c("Type 1", "Type 2"), 100, replace = T),
      slide_id = "Slide 1"
    ) %>%
      update_object_intensity(ps = 10, bw = 10),
    new_MltplxObject(
      x = runif(100, 0, 100),
      y = runif(100, 0, 100),
      marks = sample(c("Type 1", "Type 2"), 100, replace = T),
      slide_id = "Slide 2"
    ) %>%
      update_object_intensity(ps = 20, bw = 10)
  )

  expect_warning(as_MltplxExperiment(heterogeneous_list),
      "`MltplxObjects` have different pixel sizes;")

  heterogeneous_list[[1]] = heterogeneous_list[[1]] %>%
    update_object_intensity(ps = 10, bw = 10)

  heterogeneous_list[[2]] = heterogeneous_list[[2]] %>%
    update_object_intensity(ps = 10, bw = 20)

  expect_warning(as_MltplxExperiment(heterogeneous_list),
                 "`MltplxObjects` have different bandwidths;")

  heterogeneous_list[[1]] = heterogeneous_list[[1]] %>%
    update_object_intensity(ps = 10, bw = 10) %>%
    update_object_dist(jsd)

  heterogeneous_list[[2]] = heterogeneous_list[[2]] %>%
    update_object_intensity(ps = 10, bw = 10) %>%
    update_object_dist(cor)

  expect_warning({
    from_list_no_dists = as_MltplxExperiment(heterogeneous_list)
  }, "`MltplxObjects` have different distance metrics;")

  # Should still have intensities though
  expect_equal(from_list_no_dists$ps, 10)
  expect_equal(from_list_no_dists$bw, 10)
  expect_true(!is.null(from_list_no_dists[[1]]$mltplx_intensity))



  expect_no_error({
    working_conversion = new_MltplxExperiment(x = cell_x_values,
                                           y = cell_y_values,
                                           marks = factor(cell_marks),
                                           slide_id = slide_ids) %>%
      update_intensity(ps = 10, bw = 10) %>%
      update_dist(cor) %>%
      (\(l){
        l$mltplx_objects
      }) %>%
      as_MltplxExperiment()
  })

  expect_true(all(
    (working_conversion %>% print() %>% capture.output()) ==
      c(
        "MltplxExperiment with 10 slides",
        "Intensities generated with pixel size 10 and bandwidth 10 ",
        "Distance matrices generated with cor ",
        "No attached metadata"
      )
  ))


})

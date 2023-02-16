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

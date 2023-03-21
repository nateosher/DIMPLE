exp <- build_mltplx_exp(200,seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=2)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("Correct aggregation", {
  y <- unlist(lapply(exp$mltplx_objects, \(obj) obj$mltplx_dist$dist[1,2]))

  tb <- tibble(y=y,slide_id=exp$slide_ids) %>%
    left_join(exp$metadata,by="slide_id")

  # The grouped lm coefficient should just be equal to the difference in the
  # aggregate function applied to the two groups
  expected_val_max <- (tb %>% filter(group == "G2") %>% pull(y) %>% max()) -
        (tb %>% filter(group == "G1") %>% pull(y) %>% max())

  # Same with min
  expected_val_min <- (tb %>% filter(group == "G2") %>% pull(y) %>% min()) -
    (tb %>% filter(group == "G1") %>% pull(y) %>% min())

  # Same with median (which is just the mean with 2 observations)
  expected_val_median <- (tb %>% filter(group == "G2") %>% pull(y) %>% median()) -
    (tb %>% filter(group == "G1") %>% pull(y) %>% median())

  # Still doing both for good measure
  expected_val_mean <- (tb %>% filter(group == "G2") %>% pull(y) %>% mean()) -
    (tb %>% filter(group == "G1") %>% pull(y) %>% mean())

  # Now with lm_dist function
  lm_dist_max <- lm_dist(exp, "group", agg_fun = max)
  lm_dist_min <- lm_dist(exp, "group", agg_fun = min)
  lm_dist_median <- lm_dist(exp, "group", agg_fun = median)
  lm_dist_mean <- lm_dist(exp, "group", agg_fun = mean)

  # Results might not be exactly the same, but should be up to ~ 8 decimal places
  expect_true(abs(lm_dist_max$estimate[1] - expected_val_max) < 0.00000001)
  expect_true(abs(lm_dist_min$estimate[1] - expected_val_min) < 0.00000001)
  expect_true(abs(lm_dist_median$estimate[1] - expected_val_median) < 0.00000001)
  expect_true(abs(lm_dist_mean$estimate[1] - expected_val_mean) < 0.00000001)
})

test_that("lm_qdist works ", {
  set.seed(1234567)
  library(purrr)
  total_n_cells = 10000
  cell_x_values = runif(total_n_cells, 0, 600)
  cell_y_values = runif(total_n_cells, 0, 600)
  cell_marks = sample(c("Tumor", "Immune", "Other"), total_n_cells, replace = TRUE)
  slide_ids = rep(paste("Slide", 1:10), each = 1000)
  metadata = tibble(
    slide_id = paste("Slide", 1:10),
    patient_id = paste("Patient", 1:10),
    stage = sample(1:4, 10, replace = TRUE),
    age = sample(65:90, 10, replace = TRUE)
  )

  experiment = new_MltplxExperiment(x = cell_x_values,
                                    y = cell_y_values,
                                    marks = factor(cell_marks),
                                    slide_id = slide_ids,
                                    ps = 10, bw = 30,
                                    dist_metric = cor)

  #################################################
  # ERROR CATCHING

  # Check for error when metadata is absent
  expect_error(lm_qdist(experiment, "patient_id"),
               'Patient metadata must exist')

  experiment = update_metadata(experiment, metadata)

  # Nonexistant group factor
  expect_error(lm_qdist(experiment, "NONEXISTANT"),
               "Group factor must be in patient metadata")

  # No quantile dist
  expect_error(lm_qdist(experiment, "patient_id"),
               "Quantile dist must be created first")

  experiment = add_QuantileDist(experiment,
                                cor,
                                "Tumor",
                                tibble(
                                  from = c(10, 50, 90),
                                  to   = c(20, 60, 100)
                                ))
  # Missing interval
  expect_error(lm_qdist(experiment,
                        "patient_id",
                        covariates = c("stage", "age")),
               'argument "interval" is missing, with no default')

  # interval not present
  expect_error(lm_qdist(experiment,
           "patient_id",
           interval = "20-30",
           covariates = c("stage", "age")),
           "Interval must be in q_probs")

  fit_1 = lm_qdist(experiment,
           "patient_id",
           interval = "50-60",
           covariates = c("stage", "age"))

  # Estimates should be within 10e-8 of these values
  expect_true(max(abs(
    fit_1$estimate -
    c(-0.150573057,-0.085182589, 0.080804164, -0.256966637, 0.211690662,
      -0.249485109, -0.005401865, -0.377109228, 0.171203253, 0.001450565,
      0.004014972, 0.066007833, 0.001842510,  0.042187923,  0.021988589,
      -0.012300392,  0.084000367, -0.028324991, -0.106211102, -0.139636771,
      -0.122962843, -0.185411754,  0.004029413, -0.023831932, -0.112353523,
      -0.002150220, -0.003002364)
  )) < 0.00000001)

})

# test_that("agg_fun with no na.rm option is rejected", {
#   bad_fun <- function(x) sum(x)
#
#   expect_error(lm_dist(exp,"group",agg_fun = bad_fun),"na.rm must be an option in agg_fun")
# })
## TODO
## reduce_symmetric works correctly
## correct output structure (and computation) with and without covariates
## Covariates must be in metadata


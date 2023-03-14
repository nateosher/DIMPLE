exp <- build_mltplx_exp(200,seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=2)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("Correct aggregation", {
  y <- unlist(lapply(exp$mltplx_objects, \(obj) obj$mltplx_dist$dist[1,2]))

  tb <- tibble(y=y,slide_id=exp$slide_ids) %>%
    left_join(exp$metadata)

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


# test_that("agg_fun with no na.rm option is rejected", {
#   bad_fun <- function(x) sum(x)
#
#   expect_error(lm_dist(exp,"group",agg_fun = bad_fun),"na.rm must be an option in agg_fun")
# })
## TODO
## reduce_symmetric works correctly
## correct output structure (and computation) with and without covariates
## Covariates must be in metadata


exp <- build_mltplx_exp(200,seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=2)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("Correct aggregation", {
  y <- unlist(lapply(exp$mltplx_objects, \(obj) obj$mltplx_dist$dist[1,2]))
  
  tb <- tibble(y=y,slide_id=exp$slide_ids) %>%
    left_join(exp$metadata)
  
  m1 <- lm(y ~ group,data = tb) # don't aggregate to patient-level
  
  s1 <- summary(m1)
  
  m2 <- lm_dist(exp,"group")
  
  expect_false(s1$coefficients[2,1] == m2$estimate[1]) # estimate should not be the same since lm_dist aggregates
  
  tb_agg <- tb %>%
    group_by(patient_id,group) %>%
    summarise(y = median(y)) %>%
    ungroup()
  
  m1.1 <- lm(y ~ group,data=tb_agg)
  
  s1.1 <- summary(m1.1)
  
  expect_true(s1.1$coefficients[2,1] == m2$estimate[1]) # estimate should be the same
  
  m2.1 <- lm_dist(exp,"group",agg_fun = max)

  expect_false(s1.1$coefficients[2,1] == m2.1$estimate[1]) # estimate should not be the same since agg_fun is different
  
  tb_agg <- tb %>%
    group_by(patient_id,group) %>%
    summarise(y = max(y)) %>%
    ungroup()
  
  m1.2 <- lm(y ~ group,data=tb_agg)
  
  s1.2 <- summary(m1.2)
  
  expect_true(s1.2$coefficients[2,1] == m2.1$estimate[1]) # estimate should be the same
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


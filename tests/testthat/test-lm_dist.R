exp <- build_mltplx_exp(200,seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=2)

test_that("Correct linear model outputs", {
  exp <- update_intensity(exp,ps=2,bw=3)
  exp <- update_dist(exp,cor)
  
  y <- unlist(lapply(exp$mltplx_objects, \(obj) obj$mltplx_dist$dist[1,2]))
  
  tb <- tibble(y=y,slide_id=exp$slide_ids) %>%
    left_join(exp$metadata)
  
  m1 <- lm(y ~ group,data = tb) # don't aggregate to patient-level
  
  s <- summary(m1)
  
  m2 <- lm_dist(exp,"group")
  
  expect_false(s$coefficients[2,1] == m2$estimate[1]) # check to see that aggregation was done
})

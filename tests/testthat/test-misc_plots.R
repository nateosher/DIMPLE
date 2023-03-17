exp <- build_mltplx_exp(200, n_slides = 20, seed=2025)
exp <- add_mltplx_metadata(exp,n_patients=10)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("`patient_boxplots` works", {
  expect_no_error({
    boxplots_1 = patient_boxplots(exp, "X1", "X2", grouping_var = "group")
  })

  expect_equal(nrow(boxplots_1$data), 19)
  expect_equal(ncol(boxplots_1$data), 15)
  exp_no_meta = exp
  exp_no_meta$metadata = NULL

  expect_error({
    patient_boxplots(exp_no_meta, "X1", "X2", grouping_var = "group")
  },"Patient metadata must exist")
})

exp <- build_mltplx_exp(200,seed=2025)
exp <- update_intensity(exp,ps=2,bw=3)
exp <- update_dist(exp,cor)

test_that("Correct columns in df", {
  obj <- exp[[1]]
  df <- dist_to_df(obj)
  expect_true(all(c("type1","type2","dist","slide_id") %in% colnames(df)))
})

test_that("Duplication removal", {
  obj <- exp[[1]]
  df <- dist_to_df(obj)
  
  df %>%
    select(type1,type2,slide_id) %>%
    apply(.,1,sort) %>%
    t(.) %>%
    duplicated(.) -> dup_ix
  
  expect_equal(sum(dup_ix),3)
  
  df <- dist_to_df(obj,reduce_symmetric = TRUE)
  
  df %>%
    select(type1,type2,slide_id) %>%
    apply(.,1,sort) %>%
    t(.) %>%
    duplicated(.) -> dup_ix
  
  expect_equal(sum(dup_ix),0)
  
})

test_that("No NAs in df",{
  obj <- exp[[1]]
  df <- dist_to_df(obj)
  
  expect_true(sum(is.na(df$dist)) == 0)
})



exp <- build_mltplx_exp(200,seed=2025)
exp <- update_intensity(exp,ps=2,bw=3)
# setting symmetric to FALSE produces the type of distance matrix that
# this code uses to check for duplicates, i.e. a redundant/upper-lower
# distance matrix
exp <- update_dist(exp,cor, symmetric = FALSE)

# Raw data to test constructor directly
set.seed(1234567)
cell_x_values = runif(3000, 0, 600)
cell_y_values = runif(3000, 0, 600)
cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
slide_ids = rep(paste("Slide", 1:10), each = 300)
raw_data_tibble = tibble(
  x = cell_x_values,
  y = cell_y_values,
  marks = cell_marks,
  id = slide_ids
)

test_that("`MltplxObject` constructor works", {
  # Minimal constructor
  expect_no_error({
      obj_1 = new_MltplxObject(
      x = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(x),
      y = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(y),
      marks = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(marks),
      slide_id = "Slide 1"
    )
  })

  # Plot function should work
  expect_no_error({
    plot(obj_1)
  })

  print_output = capture.output(print(obj_1))
  expect_true(all(
    print_output ==
    c("MltplxObject ", "Slide id: Slide 1 ",
    "Image with 300 cells across 3 cell types",
    "Cell types: Immune, Other, Tumor ",
    "No intensity generated (yet)",
    "No distance matrix generated (yet)")
  ))

  expect_null(obj_1$mltplx_intensity)
  expect_null(obj_1$mltplx_dist)
  expect_equal(obj_1$mltplx_image$ppp$n, 300)

  # Trying to add distance matrices without intensities
  expect_error(update_object_dist(obj_1, cor, "cor"))

  # Trying to convert to dist df
  expect_equal(obj_1 %>% dist_to_df() %>% capture.output(),
               paste0("Multiplex object corresponding to slide ",
                      "id Slide 1 does not contain a distance matrix.")
              )

  # Constructor with intensities
  expect_no_error({
    obj_2 = new_MltplxObject(
      x = raw_data_tibble %>% filter(id == "Slide 2") %>% pull(x),
      y = raw_data_tibble %>% filter(id == "Slide 2") %>% pull(y),
      marks = raw_data_tibble %>% filter(id == "Slide 2") %>% pull(marks),
      slide_id = "Slide 2",
      ps = 10,
      bw = 30
    )
  })

  print_output_2 = capture.output(print(obj_2))
  expect_true(all(
    print_output_2 ==
      c("MltplxObject ",
      "Slide id: Slide 2 ",
      "Image with 300 cells across 3 cell types",
      "Cell types: Immune, Other, Tumor ",
      "Intensity generated with pixel size 10 and bandwidth 30 ",
      "No distance matrix generated (yet)")
  ))

  expect_equal(obj_2$mltplx_intensity$ps, 10)
  expect_equal(obj_2$mltplx_intensity$bw, 30)
  expect_true(all(
    dim(obj_2$mltplx_intensity$intensities) ==
      c(3600, 5)
  ))

  # Constructor with intensities + distance metric
  expect_no_error({
    obj_3 = new_MltplxObject(
      x = raw_data_tibble %>% filter(id == "Slide 3") %>% pull(x),
      y = raw_data_tibble %>% filter(id == "Slide 3") %>% pull(y),
      marks = raw_data_tibble %>% filter(id == "Slide 3") %>% pull(marks),
      slide_id = "Slide 3",
      ps = 10,
      bw = 30,
      dist_metric = cor
    )
  })

  print_output_3 = capture.output(print(obj_3))
  expect_true(all(
    print_output_3 ==
      c("MltplxObject ",
        "Slide id: Slide 3 ",
        "Image with 300 cells across 3 cell types",
        "Cell types: Immune, Other, Tumor ",
        "Intensity generated with pixel size 10 and bandwidth 30 ",
        "Distance matrix generated using cor ")
  ))

  expect_equal(obj_3$mltplx_dist$metric, "cor")
  expect_true(all(
    obj_3$mltplx_dist$cell_types ==
      c("Immune", "Other", "Tumor")
  ))

  expect_true(all(
    dim(obj_3$mltplx_dist$dist) ==
      c(3,3)
  ))

  expect_no_error({
    plot_dist_matrix(obj_3)
  })

  expect_no_error({
    plot_dist_matrix(obj_3, mode = "network")
  })

  expect_error({
    plot_dist_matrix(obj_1)
  }, "no distance matrix has been generated for this `MltplxObject`; see `update_object_dist` function")

  expect_true(all(
    colnames(obj_3$mltplx_dist$dist) ==
      c("Immune", "Other", "Tumor")
  ))

  expect_true(all(
    rownames(obj_3$mltplx_dist$dist) ==
      c("Immune", "Other", "Tumor")
  ))
})

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



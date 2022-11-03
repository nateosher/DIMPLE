test_that("hello_world() works", {
  expect_equal(hello_world("Maria"), "Hello, Maria!")
  expect_equal(hello_world("Joel"), "Hello, Joel!")
  expect_equal(hello_world("Nate"), "Hello, Nate!")
})

test_that("plastic type proportions are correct", {

  dat <- load_data()

  result <- plastic_type_summary(dat)

  expect_s3_class(result, "data.frame")

  expect_equal(result[[1, 5]],
               0.615,
               tolerance = 0.001)

  expect_equal(result[[1, 6]],
               0.2305,
               tolerance = 0.001)

  expect_equal(result[[2, 7]],
               0.178,
               tolerance = 0.001)
})

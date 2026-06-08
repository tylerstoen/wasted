test_that("load_data loads properly", {

  dat <- load_data()

  expect_shape(dat,dim=c(12034, 27))

  expect_type(dat, "list")
})

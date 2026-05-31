test_that("single country level summary works", {

  dat <- load_data()

  serbia_both <- get_country_summary(dat, "Serbia")

  montenegro_2019 <- get_country_summary(dat, "Montenegro", 2019)

  switz_2020 <- get_country_summary(dat, "Switzerland", 2020)

  # check object types
  testthat::expect_s3_class(serbia_both, "data.frame")
  testthat::expect_s3_class(montenegro_2019, "data.frame")
  testthat::expect_s3_class(switz_2020, "data.frame")

  # check column names
  testthat::expect_equal(names(serbia_both), c("country", "region","total_plastic", "population", "gdp_per_capita", "plastic_per_capita", "total_rank", "per_capita_rank", "gdp_rank", "hdpe", "ldpe", "other", "pet", "polypropylene", "polystyrene", "pvc"))

  # check cell values
  testthat::expect_equal(serbia_both[[1, 3]], 530)
  testthat::expect_equal(montenegro_2019[[1, 8]], 1)
  testthat::expect_equal(switz_2020[[1,1]], "Switzerland")

  # expect shape
  testthat::expect_shape(serbia_both, dim=c(1,16))


  # errors
  testthat::expect_error(get_country_summary(dat, "notacountry"))
  testthat::expect_error(get_country_summary(dat, "Montenegro", 1))

})

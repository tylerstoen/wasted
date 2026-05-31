test_that("filtering to country level works", {
  dat <- load_data()

  all_years <- compute_country_stats(dat)

  only_2019 <- compute_country_stats(dat, year = 2019)

  only_2020 <- compute_country_stats(dat, year = 2020)

  # check object type
  testthat::expect_s3_class(all_years, "data.frame")
  testthat::expect_s3_class(only_2019, "data.frame")
  testthat::expect_s3_class(only_2020, "data.frame")

  # check colnames
  testthat::expect_equal(names(all_years), c("country", "region","total_plastic", "population", "gdp_per_capita", "plastic_per_capita", "total_rank", "per_capita_rank", "gdp_rank"))

  # check cell values
  testthat::expect_equal(all_years[[1,3]], 3754)
  testthat::expect_equal(only_2019[[10, 1]], "Canada")
  testthat::expect_equal(round(only_2020[[8, 4]], 0), 21478690)

  # check atypical year
  testthat::expect_shape(compute_country_stats(dat, year = 1), dim = c(0,9))
})

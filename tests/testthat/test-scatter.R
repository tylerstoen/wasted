test_that("scatterplot function works", {

  dat <- load_data()

  default <- plot_plastic_vs_metric(dat)
  population <- plot_plastic_vs_metric(dat, metric = population)
  region <- plot_plastic_vs_metric(dat, region = "Europe")
  year <- plot_plastic_vs_metric(dat, region = "Asia", year = 2019)
  non_log <- plot_plastic_vs_metric(dat, log_scale = FALSE)

  testthat::expect_s3_class(default, "ggplot")
  testthat::expect_s3_class(population, "ggplot")
  testthat::expect_s3_class(region, "ggplot")
  testthat::expect_s3_class(year, "ggplot")
  testthat::expect_s3_class(non_log, "ggplot")


})

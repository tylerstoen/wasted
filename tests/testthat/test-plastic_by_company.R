test_that("plot plastic by company works", {

  dat <- load_data()

  plot_simple <- plot_plastic_by_company(dat)

  plot_top5 <- plot_plastic_by_company(
    dat,
    top_n = 5
  )

  expect_s3_class(plot_simple, "ggplot")
  expect_s3_class(plot_top5, "ggplot")
})

test_that("fit_plastic_lmer works", {

  dat <- load_data()

  model <- fit_plastic_lmer(dat)

  expect_s3_class(model, "gt_tbl")

  expect_equal(model$`_data`[[1, 2]], -0.855, tolerance = 0.001)
})


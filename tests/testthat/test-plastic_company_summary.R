test_that("plastic_by_company returns correct company totals", {

  dat <- load_data()

  result <- plastic_by_company(dat)

  expect_s3_class(result, "data.frame")

  expect_equal(
    names(result),
    c("parent_company", "total_plastic")
  )

  expect_equal(
    result$total_plastic[
      result$parent_company == "The Coca-Cola Company"
    ],
    23823
  )

  expect_equal(
    result$total_plastic[
      result$parent_company == "La Doo"
    ],
    15221
  )

})

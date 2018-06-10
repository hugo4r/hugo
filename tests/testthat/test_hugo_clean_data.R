context("Check hugo_clean_data() function")

test_that("Argument 'data' is missing",{
  expect_error(hugo_clean_data())
  expect_error(hugo_clean_data(prop = 0.1))
})

test_that("Argument 'data' is not a data.frame",{
  expect_error(hugo_clean_data(1:10))
  expect_error(hugo_clean_data(matrix(0,nrow = 3, ncol = 3)))
})

test_that("There are still NA's in data after cleaning", {
  expect_equal(sum(is.na(hugo_clean_data(airquality))), 0)
})

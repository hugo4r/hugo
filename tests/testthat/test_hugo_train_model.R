context("Check hugo_train_model() function")

test_that("Argument 'data' is not a data.frame",{
  expect_error(hugo_train_model(matrix(1,nrow=2, ncol=3)), formula)
  expect_error(hugo_train_model(seq(1,10,length.out = 100)), formula)
})

test_that("Argument 'data' is missing",{
  expect_error(hugo_train_model())
  expect_error(hugo_train_model(formula))
})

test_that("Argument 'formula' is missing",{
  expect_error(hugo_train_model())
  expect_error(hugo_train_model(data))
})


unlink("hugo_test", recursive=TRUE)

context("Check hugo_train_model() function")

test_that("Argument 'data' is not a data.frame",{
  expect_error(hugo_train_model(data = matrix(1,nrow=2, ncol=3)), formula = y~.)
  expect_error(hugo_train_model(data = seq(1,10,length.out = 100)), formula = y~.)
})

test_that("Argument 'data' is missing",{
  expect_error(hugo_train_model())
  expect_error(hugo_train_model(formula = y~.))
})

test_that("Argument 'formula' is missing",{
  expect_error(hugo_train_model())
  expect_error(hugo_train_model(data = data.frame(class=c(1,1,2,2), value = c(2,3,4,5))))
})


unlink("hugo_test", recursive=TRUE)

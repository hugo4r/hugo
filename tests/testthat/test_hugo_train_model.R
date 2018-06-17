context("Check hugo_train_model() function")

test_that('hugo_start_investigation() has been executed', {
  data("PimaIndiansDiabetes")
  formula <- "diabetes~."
  expect_error(hugo_train_model(PimaIndiansDiabetes, formula), 'Call hugo_start_investigation\\(\\) for starting the new investigation.')
})

hugo_start_investigation("hugo_test")

test_that('Function takes arguments', {
  expect_error(hugo_train_model(data = NULL, formula = NULL), "There are no objects to training.")
})

test_that('The function takes arguments - data', {
  expect_error(hugo_train_model(formula = "Species~.", data = NULL), "There is no data to training models.")
})

test_that('The function takes arguments - formula', {
  expect_error(hugo_train_model(data = iris, formula = NULL), "There is no formula to training models.")
})

test_that("Argument 'data' is not a data.frame", {
  data("PimaIndiansDiabetes")
  expect_error(hugo_train_model(data = as.matrix(PimaIndiansDiabetes), formula = "diabetes~."), "Data is not a data.frame.")
})

test_that("Argumnet 'formula' is incorrect", {
  data("PimaIndiansDiabetes")
  expect_error(hugo_train_model(data = PimaIndiansDiabetes, formula = "diabetes + age"), "The formula is incorrect.")
})

test_that("Argument 'formula' is not a formula", {
  data("PimaIndiansDiabetes")
  expect_error(hugo_train_model(data = PimaIndiansDiabetes, formula = "diabetes."), "The formula is incorrect.")
})

test_that("Response variable has more than two classes",{
  expect_error(hugo_train_model(data = iris, formula = "Species~."), "Response variable has more than two classes. Incorrect data to binary classification.")
})

test_that('There is a memory subdirectory after training models.', {
  data("PimaIndiansDiabetes")
  data <- PimaIndiansDiabetes
  formula <- "diabetes~."
  hugo_train_model(data, formula)
  expect_true('models' %in% list.files('./hugo_test'))
  expect_true("glm_model.rda" %in% list.files("./hugo_test/models"))
  expect_true("randomforest_model.rda" %in% list.files("./hugo_test/models"))
  expect_true("gbm_model.rda" %in% list.files("./hugo_test/models"))
})

unlink("hugo_test", recursive=TRUE)

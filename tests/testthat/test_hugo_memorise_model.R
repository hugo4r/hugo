context("Check hugo_memorise_model() function")

hugo_start_investigation("hugo_test")

test_that("model and summary is saved without warnings/errors", {
  expect_output(hugo_memorise_model(lm(c(1,2,3)~c(2,3,1)), name = "test"))
})

test_that("NULL is not saved", {
  expect_error(hugo_memorise_model())
})

test_that("if object is not a model summary is not created", {
  a <- 1
  expect_error(hugo_memorise_model(a, name = "test"))
})

test_that("there is a directory gallery", {
  expect_true("./hugo_test/gallery" %in% list.dirs())
})

test_that("model is saved", {
  expect_true({
    hugo_memorise_model(lm(c(1,2,3)~c(2,3,1)), name = "test")
    "test.rda" %in% list.files("./hugo_test/gallery")
  })
})

test_that("summary is saved", {
  expect_true({
    hugo_memorise_model(lm(c(1,2,3)~c(2,3,1)), name = "test")
    "test.md" %in% list.files("./hugo_test/gallery")
    })
})

unlink("hugo_test", recursive = TRUE)

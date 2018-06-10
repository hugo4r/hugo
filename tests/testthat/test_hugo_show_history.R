context("Check hugo_show_history() function")

test_that("error when using any arguments", {
  expect_error(hugo_show_history("aaa"))
  expect_error(hugo_show_history(c(1,2)))
  expect_error(hugo_show_history("aaa",12,list("b",1)))
})
context("Check hugo_show_history() function")

test_that("error when using wrong arguements", {
  expect_error(hugo_show_history("aaa"))
  expect_error(hugo_show_history(c(1,2)))
  expect_error(hugo_show_history("aaa",12,c(1,1)))
  expect_error(hugo_show_history(1.5))
  expect_error(hugo_show_history(sqrt(7)))
})

test_that("error when using too many arguements", {
  expect_error(hugo_show_history(1,2,3))
  expect_error(hugo_show_history(first=1,specyfic=c(2,7)))
  expect_error(hugo_show_history(last=1,specyfic=c(2,7)))
})

test_that("success when expected to success", {
  expect_failure(expect_error(hugo_show_history(5)))
  expect_failure(expect_error(hugo_show_history(first=10)))
  expect_failure(expect_error(hugo_show_history(specyfic=c(-10))))
  expect_failure(expect_error(hugo_show_history(specyfic=c(1,5,10))))
})

test_that("arg restart=T restarts history", {
  expect_failure(expect_error(hugo_show_history(restart=T)))
  expect_equal(.hugoEnv$history,"empty")
})

test_that("there are correct entries in history", {
  expect_error(hugo_show_history(5,4))
  expect_equal(.hugoEnv$history[length(.hugoEnv$history)],"hugo_show_history(last = 5, first = 4)")
 
})

context("Check hugo_start_investigation() function")

hugo_start_investigation("hugo_test")

test_that("there is a .session_info", {
  expect_true(".session_info" %in% list.files("hugo_test",all.files = T))
})

test_that("there is a directory", {
  expect_true("./hugo_test" %in% list.dirs())
})

unlink("hugo_test", recursive = TRUE)

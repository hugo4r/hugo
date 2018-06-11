context("Check hugo_save_investigation() function")

hugo_start_investigation("hugo_test")

test_that("no variables for saving returns warning", {
  expect_warning(hugo_save_investigation())
})

test_that("no defined variable throws error", {
  expect_error(hugo_save_investigation(variables = "a"))
})

test_that("environment not exist", {
  expect_error(hugo_save_investigation(envir = s))
})

test_that("basic funcionality works", {
  e <- new.env()
  e$a <- FALSE
  e$b <- "a"
  expect_output(hugo_save_investigation(envir = e, session_name = "test_session"))
})

test_that("there are directories", {
  expect_true("./hugo_test/resources" %in% list.dirs())
  expect_true("./hugo_test/resources/test_session" %in% list.dirs())
})


test_that("there are saved files", {
  expect_true("packages" %in% list.files("./hugo_test/resources/test_session", all.files = T))
  expect_true("variables" %in% list.files("./hugo_test/resources/test_session", all.files = T))
})

test_that("it is possible to restore the data", {
  e <- new.env()
  e$a <- 5
  hugo_save_investigation(session_name = "forLoad", envir = e)
  load("./hugo_test/resources/forLoad/variables", envir = e)
  expect_true(e$a == 5)
})


#test_that("hugo asks for overwriting session", {
#  e <- new.env()
#  e$a <- "Hugo"
#  expect_output(hugo_save_investigation(envir = e, session_name = "test_session"))
#})

unlink("hugo_test", recursive = TRUE)



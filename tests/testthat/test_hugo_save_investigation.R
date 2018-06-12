context("Check hugo_save_investigation() function")


test_that("investigation will start - input", {
  .hugoEnv$path <- 'unusedpath'

  f <- file()
  options(hugo.connection_in = f)
  ans <- "hugo_test"
  write(ans, f)
  expect_warning(hugo_save_investigation(), "No variables to save.")
  options(hugo.connection_in = stdin())
  close(f)

})

test_that("there is a directory", {
  expect_true("./hugo_test" %in% list.dirs())
})

hugo_start_investigation("hugo_test")

test_that("no variables for saving returns warning", {
  expect_warning(hugo_save_investigation(), "No variables to save.")
})

test_that("no defined variable throws error", {
  expect_error(hugo_save_investigation(variables = "a"), '^Following varables are not defined:')
})

test_that("environment not exist", {
  expect_error(hugo_save_investigation(envir = s))
})

test_that("basic funcionality works", {
  e <- new.env()
  e$a <- FALSE
  e$b <- "a"
  expect_output(hugo_save_investigation(envir = e, session_name = "test_session"), "*succesfully saved")
})

test_that("selected variables are saved", {
  e <- new.env()
  e$a <- FALSE
  e$b <- "a"
  expect_output(hugo_save_investigation(variables = "b", envir = e))
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


test_that("input default", {

  f <- file()
  options(hugo.connection_in = f)
  ans <- "0"
  write(ans, f)
  e <- new.env()
  e$a <- 5
  expect_output(hugo_save_investigation(envir = e, session_name = "newsession"))
  expect_true("variables" %in% list.files("./hugo_test/resources/newsession", all.files = T))
  options(hugo.connection_in = stdin())
  close(f)

})


test_that("input overwritting", {

  f <- file()
  options(hugo.connection_in = f)
  ans <- "newname"
  write(ans, f)
  e <- new.env()
  e$a <- 5
  expect_output(hugo_save_investigation(envir = e, session_name = "newsession"))
  expect_true("variables" %in% list.files("./hugo_test/resources/newname", all.files = T))
  options(hugo.connection_in = stdin())
  close(f)

})

unlink("hugo_test", recursive = TRUE)



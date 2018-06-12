context("Check hugo_continue_investigation() function")

hugo_start_investigation("hugo_test_continue")

test_that("No saved sessions exist",{

  expect_error(hugo_continue_investigation())

})

test_that("Variables are loaded",{

  e <- new.env()
  e$a <- 5
  hugo_save_investigation(session_name = "test_session",envir = e)
  remove(a, envir=e)
  hugo_continue_investigation( session_name = "test_session", envir = e)
  expect_equal(e$a,5)

})

test_that("Single session is loaded",{

  expect_error(hugo_continue_investigation(),NA) #Expect success

})


test_that("Specified session is loaded",{

  e <- new.env()
  e$b <- 5
  hugo_save_investigation(session_name="test_session 2",envir = e)
  remove(b,envir=e)
  hugo_continue_investigation(session_name = "test_session 2",envir = e)
  expect_equal(e$b,5)

})


test_that("Multiple sessions handling",{

  e <- new.env()
  e$b <- 5
  hugo_save_investigation(session_name="test_session 3",envir = e)
  remove(b,envir = e)
  f <- file()
  options(hugo.connection_in = f)
  ans <- "test_session 3"
  write(ans,f)
  hugo_continue_investigation(envir=e)
  options(hugo.connection_in = stdin())
  close(f)
  expect_equal(e$b,5)

})

test_that("Invalid session name is provided",{

  expect_error(hugo_continue_investigation(session_name="Invalid"))
})

test_that("Missing package is loaded",{

  library(datasets)
  hugo_save_investigation(session_name="test_session 4")
  detach(name="package:datasets", unload=TRUE, force = TRUE)
  hugo_continue_investigation(session_name="test_session 4")
  expect_true("datasets" %in% .packages())
})

test_that("Missing package is installed",{

  if(!require(astsa)){
    install.packages("astsa")
    library(astsa)
  }
  hugo_save_investigation(session_name = "test_session 5")
  detach(name="package:astsa", unload=TRUE, force = TRUE)
  remove.packages("astsa")
  hugo_continue_investigation(path="hugo_test_continue",session_name="test_session 5")
  expect_true("astsa" %in% .packages())

})


test_that("Missing package cannot be installed",{

  utils::install.packages("testpackage_0.0.0.9000.tar.gz", repos = NULL, type="source")
  library(testpackage)
  hugo_save_investigation(session_name = "test_session 6")
  detach(name="package:testpackage", unload=TRUE, force = TRUE)
  remove.packages("testpackage")
  expect_error(hugo_continue_investigation(path="hugo_test_continue",session_name="test_session 6"),NA)
})

unlink("hugo_test_continue", recursive = TRUE)

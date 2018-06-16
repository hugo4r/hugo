context("Check hugo_continue_investigation() function")

hugo_start_investigation("hugo_test_continue")

test_that("No saved sessions exist",{

  expect_error(hugo_continue_investigation())

})

test_that("Variables are loaded",{

  test_environment <- new.env()
  test_environment$test_variable <- 5
  hugo_save_investigation(session_name = "test_session",envir = test_environment)
  remove(test_variable, envir=test_environment)
  hugo_continue_investigation( session_name = "test_session", envir = test_environment)
  expect_equal(test_environment$test_variable,5)

})

test_that("Single session is loaded",{

  test_environment <- new.env()
  expect_error(hugo_continue_investigation(envir=test_environment),NA) #Expect success

})


test_that("Specified session is loaded",{

  test_environment <- new.env()
  test_environment$test_variable <- 5
  hugo_save_investigation(session_name="test_session 2",envir = test_environment)
  remove(test_variable,envir=test_environment)
  hugo_continue_investigation(session_name = "test_session 2",envir = test_environment)
  expect_equal(test_environment$test_variable,5)

})


test_that("Multiple sessions handling",{

  test_environment <- new.env()
  test_environment$test_variable <- 5
  hugo_save_investigation(session_name="test_session 3",envir = test_environment)
  remove(test_variable,envir = test_environment)
  test_connection <- file()
  options(hugo.connection_in = test_connection)
  answer <- "test_session 3"
  write(answer,test_connection)
  hugo_continue_investigation(envir=test_environment)
  options(hugo.connection_in = stdin())
  close(test_connection)
  expect_equal(test_environment$test_variable,5)

})

test_that("Invalid session name is provided",{

  test_environment <- new.env()
  expect_error(hugo_continue_investigation(session_name="Invalid",envir=test_environment))
})

test_that("Missing package is loaded",{

  test_environment <- new.env()
  library(datasets)
  hugo_save_investigation(session_name="test_session 4",envir=test_environment)
  detach(name="package:datasets", unload=TRUE, force = TRUE)
  hugo_continue_investigation(session_name="test_session 4",envir=test_environment)
  expect_true("datasets" %in% .packages())
})
test_that("Missing package cannot be installed",{

  test_environment <- new.env()
  utils::install.packages("testpackage_0.0.0.9000.tar.gz", repos = NULL, type="source")
  library(testpackage)
  hugo_save_investigation(session_name = "test_session 6",envir=test_environment)
  detach(name="package:testpackage", unload=TRUE, force = TRUE)
  remove.packages("testpackage")
  expect_error(hugo_continue_investigation(path="hugo_test_continue",session_name="test_session 6",envir=test_environment),NA)
})

unlink("hugo_test_continue", recursive = TRUE)

context("Check hugo_summarise_data() function")


test_that('successfully generating report',{
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)

  ans <- paste(c('pdf','TRUE','FALSE','all', 'TRUE', '2', 'iris', 'iris',
                 'pdf','TRUE','FALSE','all', 'TRUE', '2', 'iris', 'iris',
                 'iris', 'iris',
                 'pdf','TRUE','FALSE','all', 'TRUE', '2', 'cars', 'cars'),collapse = '\n')
  write(ans,f)
  hugo_start_investigation("hugo_test")
  expect_output(hugo_summarise_data(iris, FALSE), "Success!")
  expect_output(hugo_summarise_data(iris), "Success!")
  expect_output(with_mock(
    menu_prev_settings = function(default_settings){return(TRUE)},
    hugo_summarise_data(iris)), "Success!")

  expect_output(with_mock(
    menu_prev_settings = function(default_settings){return(FALSE)},
    hugo_summarise_data(cars)), "Success!")

  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})


test_that('wrong type of data',{
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)

  ans <- paste(c(c('pdf','TRUE','FALSE','all', 'TRUE', '2', 'iris', 'iris',
                   'iris', 'iris')),collapse = '\n')
  write(ans,f)

  hugo_start_investigation("hugo_test")
  expect_error(hugo_summarise_data("dane"))




  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})

test_that('wrong type of overwrite parameters',{


  hugo_start_investigation("hugo_test")
  expect_error(hugo_summarise_data(iris, "true"))
  expect_error(hugo_summarise_data(iris, 23))

})


test_that('wrong type of typed parameters',{
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  options(hugo.use_summary_parameters = T)
  options(hugo.use_default_name_summary_parameters = T)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)
  ans <- paste(c('word','TRUE','FALSE','all', 'TRUE', '2', 'iris', 'iris',
                 'html','null','FALSE','all', 'TRUE', '2', 'cars', 'cars',
                 'html','TRUE','false','all', 'TRUE', '2', 'cars', 'cars',
                 'html','TRUE','FALSE','all', 't', '2', 'cars', 'cars',
                 'html','TRUE','FALSE','all', 'TRUE', 'null', 'cars', 'cars'),collapse = '\n')
  write(ans,f)

  expect_message(hugo_summarise_data(data = iris), "Incorrect format of output. Default html will be used.")
  expect_message(with_mock(
    menu_prev_settings = function(default_settings){return(FALSE)},
    hugo_summarise_data(data = cars))
  , "Incorrect value. Default TRUE will be used.")

  expect_message(with_mock(
    menu_prev_settings = function(default_settings){return(FALSE)},
    hugo_summarise_data(data = cars))
    , "Incorrect value. Default TRUE will be used.")

  expect_message(with_mock(
    menu_prev_settings = function(default_settings){return(FALSE)},
    hugo_summarise_data(data = cars))
    , "Incorrect value. Default TRUE will be used.")

  expect_message(with_mock(
    menu_prev_settings = function(default_settings){return(FALSE)},
    hugo_summarise_data(data = cars))
    , "Incorrect value. Default 2 will be used.")


  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})

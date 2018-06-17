context("Check hugo_summarise_data() function")




test_that('successfully generating report',{
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)

  ans <- paste(c( '3', 'iris', 'iris',
                  'iris', 'iris',
                  '4', 'iris', 'iris',
                  '2', 'iris', 'iris',
                 'iris', 'iris',
                  '2', 'cars', 'cars',
                 '2', 'NULL','NULL'),collapse = '\n')
  write(ans,f)
  hugo_start_investigation("hugo_test")
  expect_output(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){'pdf'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(iris, FALSE)), "Success!")


  expect_output(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){TRUE},
    hugo_summarise_data(iris, FALSE)), "Success!")

  expect_output(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){'pdf'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(iris)), "Success!")

  expect_output(with_mock(
    "hugo:::menu_prev_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){'pdf'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(iris)), "Success!")

  expect_output(with_mock(
    "hugo:::menu_prev_settings" = function(default_parameters, names_parameters){return(TRUE)},
    hugo_summarise_data(iris)), "Success!")

  expect_output(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Success!")

  expect_output(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(data.frame(a = c(1,2), b=c(23,34)))), "Success!")

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
  ans <- paste(c( '3', 'iris', 'iris',
                  '2', 'iris', 'iris'),collapse = '\n')
  write(ans,f)
  hugo_start_investigation("hugo_test")
  expect_error(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){'pdf'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data("dane", FALSE)))

  expect_error(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(25)))

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


test_that('wrong type of choosen parameters',{
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  options(hugo.use_summary_parameters = T)
  options(hugo.use_default_name_summary_parameters = T)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)
  ans <- paste(c( '3', 'iris', 'iris',
                  '2', 'cars', 'cars',
                  '2', 'cars', 'cars',
                  '2', 'iris', 'iris'),collapse = '\n')
  write(ans,f)
  hugo_start_investigation("hugo_test")
  expect_message(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){NULL},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(iris)), "Default value will be used: html")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){NULL},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Default value will be used: TRUE")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){NULL},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Default value will be used: FALSE")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){NULL},
    hugo_summarise_data(iris)), "Default value will be used: TRUE")


  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
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
  ans <- paste(c( '30', 'iris', 'iris',
                  'dwa', 'cars', 'cars',
                  '2.5', 'cars', 'cars',
                  '-2', 'iris', 'iris',
                  '2', '', 'iris',
                  '2', 'NULL', 'iris',
                  '2', 'cars', '',
                  '3', 'cars', 'NULL'
                  ),collapse = '\n')
  write(ans,f)
  hugo_start_investigation("hugo_test")
  expect_message(with_mock(
    "hugo:::menu_first_settings" = function(default_parameters, names_parameters){FALSE},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(iris)), "Incorrect value. Default 2 will be used.")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Incorrect value. Default 2 will be used.")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Incorrect value. Default 2 will be used.")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Incorrect value. Default 2 will be used.")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Default filename will be used")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Default filename will be used")

  expect_message(with_mock(
    "hugo:::menu_prev_settings" = function(default_settings, names_parameters){return(FALSE)},
    "hugo:::get_output_from_user" = function(){'html'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(cars)), "Default title will be used")


  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})


test_that('testing menu  for choice',{
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  options(hugo.use_summary_parameters = T)
  options(hugo.use_default_name_summary_parameters = T)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)
  ans <- paste(c( 'iris', 'iris',
                  '3', 'iris_2', 'iris'),collapse = '\n')
  write(ans,f)
  hugo_start_investigation("hugo_test")
  expect_output(with_mock(
    "hugo:::hugo_choose_menu" = function(text_options, value_options, title){value_options[1]},
     hugo_summarise_data(iris)), "Success")
  expect_output(with_mock(
    "hugo:::hugo_choose_menu" = function(text_options, value_options, title){value_options[2]},
    "hugo:::get_output_from_user" = function(){'pdf'},
    "hugo:::get_replace_from_user" = function(){TRUE},
    "hugo:::get_open_from_user" = function(){FALSE},
    "hugo:::get_smart_factor_from_user" = function(){TRUE},
    hugo_summarise_data(iris)), "Success")


  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})




requireNamespace_mock <- function(package, quietly) {
  return(false)
}

test_that("throws error when there is no \"dataMaid\" package", {
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  options(hugo.use_summary_parameters = T)
  options(hugo.use_default_name_summary_parameters = T)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)
  ans <- paste(c( 'iris', 'iris',
                   'iris_2', 'iris'),collapse = '\n')
  write(ans,f)
  with_mock(requireNamespace = requireNamespace_mock,
            "hugo:::hugo_choose_menu" = function(text_options, value_options, title){value_options[1]},
            expect_error(hugo_summarise_data(iris))
  )
  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})


requireNamespace_mock <- function(package, quietly) {
  return(false)
}

test_that("throws error when there is no \"rmarkdown\" package", {
  tmp <- getOption("hugo.know_summary_parameters")
  options(hugo.know_summary_parameters = F)
  options(hugo.use_summary_parameters = T)
  options(hugo.use_default_name_summary_parameters = T)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)
  ans <- paste(c( 'iris', 'iris',
                  'iris_2', 'iris'),collapse = '\n')
  write(ans,f)
  with_mock(requireNamespace = requireNamespace_mock,
            "hugo:::hugo_choose_menu" = function(text_options, value_options, title){value_options[1]},
            expect_error(hugo_summarise_data(iris))

  )
  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_summary_parameters = tmp)
  close(f)
  close(g)
})


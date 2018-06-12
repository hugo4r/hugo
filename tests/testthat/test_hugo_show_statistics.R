context("Check hugo_show_statistics() function")

hugo_start_investigation("test_hugo_show_statistics")

test_that("Summaries correctly created", {
  expect_output(hugo_show_statistics())
})

test_that("Invalid number of parameters",{
  expect_error(hugo_show_statistics("test_hugo_show_statistics"))
  expect_error(hugo_show_statistics(".hugoEnv$path"))
})

test_that("Correct results",{
  model <- lm(Sepal.Length ~ Petal.Length, data = iris)
  hugo_memorise_model(model)
  hugo_memorise_plot(plot(rnorm(10), rnorm(10)))
  alphabet <- letters
  hugo_memorise(alphabet)

  statistics <- hugo_show_statistics()
  expect_equal(statistics$creationDate, toString(Sys.Date()))
  expect_equal(statistics$models, 1)
  expect_equal(statistics$objects, 1)
})

test_that("Wrong input",{
  expect_error(hugo_show_statistics("test_hugo_show_statistics"))
  expect_error(hugo_show_statistics(".hugoEnv$path"))
})

unlink("test_hugo_show_statistics", recursive = TRUE)


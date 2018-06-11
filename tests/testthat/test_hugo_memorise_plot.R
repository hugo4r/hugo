context("Check hugo_memorise_plot() function")

hugo_start_investigation("hugo_test")

test_that("have to put a plot", {
  expect_error(hugo_memorise_plot())
})

test_that("wrong type of object in input", {
  expect_error(hugo_memorise_plot(1))
})

test_that("error in creating plot", {
  expect_error(hugo_memorise_plot(plot(1:10,1:5)))
})

test_that("plot created with package graphics is correctly saved without name in input", {
  expect_output(hugo_memorise_plot(plot(1:10)))
})

test_that("plot created with more then one function from package graphics is correctly saved", {
  expect_output(hugo_memorise_plot({plot(1:10)
    abline(a = 1, b = 1)}))
})

test_that("function without returning value", {
  expect_null(hugo_memorise_plot(plot(1:10)))
})

test_that("plot created with package ggplot2 is correctly saved with name in input", {
  df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),y = rnorm(30))
  require(ggplot2)
  plot_gg <- ggplot(df, aes(gp, y)) + geom_point()
  expect_output(hugo_memorise_plot(plot_gg, "test_name"))
})

test_that("directory was created", {
  expect_true("./hugo_test/gallery" %in% list.dirs())
})

test_that("object saved as .rda",{
  hugo_memorise_plot(plot(1:10), "testrda")
  expect_true("testrda.rda" %in% list.files("./hugo_test/gallery"))
})

test_that("Can't double names",{
  hugo_memorise_plot(plot(1:10), "test")
  expect_error(hugo_memorise_plot(plot(1:10), "test"))
})

test_that("object without name is saved",{
  hugo_memorise_plot(plot(1:10))
  expect_false(length(list.files("./hugo_test/gallery", pattern = "(plot)")) == 0)
})

test_that("object without name on input are saved with next numbers",{
  hugo_memorise_plot(plot(1:5))
  hugo_memorise_plot(plot(1:10, type = "l"))
  numb_of_plots <- substr(list.files("./hugo_test/gallery", pattern = "(plot)"), start = 5L, stop = 6L)
  numb_of_plots <- unique(as.numeric(numb_of_plots)[!is.na(as.numeric(numb_of_plots))])[order(unique(as.numeric(numb_of_plots)[!is.na(as.numeric(numb_of_plots))]), decreasing = TRUE)]
  expect_true(numb_of_plots[1] == numb_of_plots[2] + 1)
})

test_that("Next numbers when more then 9 plots in folder",{
  hugo_memorise_plot(plot(1:10), "plot10")
  hugo_memorise_plot(plot(1:10))
  numb_of_plots <- substr(list.files("./hugo_test/gallery", pattern = "(plot)"), start = 5L, stop = 6L)
  expect_true(max(unique(as.numeric(numb_of_plots)[!is.na(as.numeric(numb_of_plots))])) == 11)
 })

unlink("hugo_test", recursive = TRUE)

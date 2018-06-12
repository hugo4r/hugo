context("Check hugo_clean_data() function")

test_that("Argument 'data' is missing",{
  expect_error(hugo_clean_data())
  expect_error(hugo_clean_data(prop = 0.1))
})

test_that("Argument 'data' is not a data.frame",{
  expect_error(hugo_clean_data(1:10))
  expect_error(hugo_clean_data(matrix(0,nrow = 3, ncol = 3)))
})

test_that("There should not be NA's in data after cleaning", {
  expect_equal(sum(is.na(hugo_clean_data(airquality))), 0)
})

test_that("Filling NA's for data with factors (one mode)", {
  expect_equal({
    data <- Puromycin
    data[12, "state"] <- NA
    sum(is.na(hugo_clean_data(data)))
    }, 0)
})

test_that("Filling NA's for data with factors (two modes)", {
  expect_equal({
    data <- Puromycin
    data[12:15, "state"] <- NA
    sum(is.na(hugo_clean_data(data)))
  }, 0)
})

test_that("Test interaction with users",{
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)

  ans <- paste(c('4','-2','0.5'),collapse = '\n')
  write(ans,f)

  expect_equal(sum(is.na(hugo_clean_data(airquality, 5))), 0)

  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  close(f)
  close(g)
})

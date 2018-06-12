context('Check hugo_memorise() function')


test_that('hugo_start_investigation() has been executed', {
  test1 <- 1:3
  expect_error(hugo_memorise(test1), 'Call hugo_start_investigation\\(\\) for starting the new investigation.')
})

hugo_start_investigation('hugo_test')

test_that('the function takes an argument', {
  expect_error(hugo_memorise(), 'There is no object to be saved.')
})

test_that('the function takes exactly one argument', {
  test2 <- 1:3
  test3 <- 4:6
  expect_error(hugo_memorise(test2,test3))
})

test_that('an object is saved without any warnings/errors', {
  test4 <- list('abcdef', seq(0,5,0.1), c(TRUE,FALSE))
  expect_output(hugo_memorise(test4), cat('Copy of the \"test4\" object is stored in hugo_test/memory/.\n'))
})

test_that('there is a memory subdirectory after saving an object', {
  test5 <- 1:3
  hugo_memorise(test5)
  expect_true('memory' %in% list.files('./hugo_test'))
})

test_that('an object is saved correctly', {
  test6 <- 1:3
  hugo_memorise(test6)
  expect_true('test6.rda' %in% list.files('./hugo_test/memory'))
})

test_that('not overwriting the object that already exists', {
  test7 <- 1:3
  hugo_memorise(test7)
  f <- file()
  options(hugo.connection_in = f)
  ans <- 2
  write(ans, f)
  test7 <- 4:6
  expect_output(hugo_memorise(test7), cat('The \"test7\" object was not copied.\n'))
  options(hugo.connection_in = stdin())
  close(f)
})

test_that('overwriting an object', {
  test8 <- 1:3
  hugo_memorise(test8)
  f <- file()
  options(hugo.connection_in = f)
  ans <- 1
  write(ans, f)
  test8 <- 4:6
  expect_output(hugo_memorise(test8), cat('Copy of the \"test8\" object is stored in hugo_test/memory/.\n'))
  options(hugo.connection_in = stdin())
  close(f)
})


unlink('hugo_test', recursive = TRUE)




context('Check hugo_memorise() function')

hugo_start_investigation('hugo_test')


test_that('the function takes argument', {
  expect_error(hugo_memorise(), 'There is no object to be saved.')
})

test_that('the function takes exactly one argument', {
  a <- 1:3
  b <- 4:6
  expect_error(hugo_memorise(a,b))
})

test_that('an object is saved without warnings/errors', {
  a1 <- 1:3
  expect_output(hugo_memorise(a1), cat('Copy of the \"a1\" object is stored in hugo_memorise_test/memory/.\n'))
})

test_that('there is a memory subdirectory after saving an object', {
  a2 <- 1:3
  hugo_memorise(a2)
  expect_true('memory' %in% list.files('./hugo_test'))
})

test_that('an object is saved correctly', {
  a3 <- 1:3
  hugo_memorise(a3)
  expect_true('a3.rda' %in% list.files('./hugo_test/memory'))
})

unlink('hugo_test', recursive = TRUE)







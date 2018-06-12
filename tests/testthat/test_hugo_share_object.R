context("Check hugo_share_object() and hugo_get_object() functions")


test_that('properly loads object from example 1',{
  object_1 <- hugo_get_object('tidyverse','readr','revdep/checks.rds')
  expect_equal(length(object_1),4)
})

test_that('properly loads object from example 2',{
  object_2 <- hugo_get_object('hadley','r-python','github.rds')
  expect_equal(colnames(object_2),c("year", "month","start","end", "lang", "query", "count"))
})

test_that('fails when given object is not in repository',{
  expect_error(hugo_get_object('hadley','r-python','i_am_very_shure_this_is_not_a_actual_file.rds'))
})

test_that('fails when there is no such repository',{
  expect_error(hugo_get_object('MatKru','there_is_no_such_repository_for_shure','file.rds'))
})

test_that('sharing fails with wrong credentials',{
  tmp <- getOption("hugo.know_credentials")
  options(hugo.know_credentials = F)
  f <- file()
  g <- file()
  options(hugo.connection_in = f)
  options(hugo.connection_out = g)
  ans <- paste(c('admin','password','repo_name'),collapse = '\n')
  write(ans,f)

  expect_error(hugo_share_object(iris))

  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_credentials = tmp)
  close(f)
  close(g)
})




requireNamespace_mock <- function(package, quietly) {
  return(false)
}

test_that("throws error when there is no \"git2r\" package", {
  with_mock(requireNamespace = requireNamespace_mock,
            expect_error(hugo_get_object('user','repo_name','object_name'))
  )
})

test_that("throws error when there is no \"git2r\" package", {
  with_mock(requireNamespace = requireNamespace_mock,
            expect_error(hugo_share_object(iris))
  )
})

test_that("throws error when there is no \"getPass\" package", {
  with_mock(requireNamespace = requireNamespace_mock,
            expect_error(hugo_share_object(iris))
  )
})

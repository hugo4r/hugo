context("Check hugo_share_object() function")


test_that('properly loads object from example 1',{
  object_1 <- hugo_get_object('tidyverse','readr','revdep/checks.rds')
  expect_equal(length(object_1),4)
})

test_that('properly loads object from example 2',{
  object_2 <- hugo_get_object('hadley','r-python','github.rds')
  expect_equal(colnames(object_2),c("year", "month","start","end", "lang", "query", "count"))
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



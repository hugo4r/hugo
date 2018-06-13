# context("Check hugo_memorise_table() function")
#
# hugo_start_investigation("hugo_test")
#
# test_that("NULL is not saved", {
#   expect_error(hugo_memorise_table())
# })
#
# test_that("object is not created if it is not a table", {
#   a <- 1
#   expect_error(hugo_memorise_table(a, name = "table"))
# })
#
# test_that("there is a directory gallery", {
#   expect_true("./hugo_test/gallery" %in% list.dirs())
# })
#
# test_that("table is saved", {
#   expect_true({
#     hugo_memorise_table(iris, name = "table")
#     "table.rda" %in% list.files("./hugo_test/gallery")
#   })
# })
#
# test_that("tables are saved without warnings/errors", {
#   expect_output(hugo_memorise_table(iris, name = "table"))
# })
#
# test_that("markdown table is saved", {
#   expect_true({
#     hugo_memorise_table(iris, name = "table")
#     "table.md" %in% list.files("./hugo_test/gallery")
#   })
# })
#
# unlink("hugo_test", recursive = TRUE)

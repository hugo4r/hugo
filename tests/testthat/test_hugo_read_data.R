context("Check hugo_read_data() function")

hugo_start_investigation("hugo_test")

test_that("No file path provided.",{
  expect_error(hugo_read_data(path=NULL))
})

test_that("File doesn't exist.",{
  expect_error(hugo_read_data(path="dummy_file.csv"))
})

test_that("File extension is not supported.",{
  expect_error(hugo_read_data(path="bad_extension.abc"))
})

test_that("Must of providing all parameters or none.",{
  expect_error(hugo_read_data(path="example2.txt", header=TRUE))
  expect_error(hugo_read_data(path="example2.txt", header=TRUE, separator = ";"))
  expect_error(hugo_read_data(path="example2.txt", decimal = "."))
})

# test_that("File without extension handled.", {
#   data <- hugo_read_data(path="no_extension")
#   expect_equal(dim(data), c(35,44))
#   expect_true("data.frame" %in% class(data))
# })

test_that("Json file loaded correctly.",{
  data <- hugo_read_data(path="https://raw.githubusercontent.com/corysimmons/colors.json/master/colors.json")
  expect_equal(length(data), c(149))
  expect_true("list" %in% class(data))
})

test_that("Rda file loaded correctly.",{
  data <- hugo_read_data(path="example.rda")
  expect_equal(dim(data), c(150,5))
  expect_true("data.frame" %in% class(data))
})

test_that("Tsv file loaded correctly.",{
  data <- hugo_read_data(path="example.tsv")
  expect_equal(dim(data), c(3,3))
  expect_true("data.frame" %in% class(data))
})

test_that("Csv file loaded correctly.",{
  data <- hugo_read_data(path="example3.csv")
  expect_equal(dim(data), c(500,12))
  expect_true("data.frame" %in% class(data))

  data2 <- hugo_read_data(path="example2.csv")
  expect_equal(dim(data2), c(985,12))
  expect_true("data.frame" %in% class(data2))
})

test_that("Txt file loaded correctly.",{
  data <- hugo_read_data(path="example.txt")
  expect_equal(dim(data), c(35,44))
  expect_true("data.frame" %in% class(data))

  data2 <- hugo_read_data(path="example2.txt")
  expect_equal(dim(data2), c(14690,3))
  expect_true("data.frame" %in% class(data2))
})

test_that("Xls and xlsx files loaded correctly.",{
  # data <- hugo_read_data(path="example.xls")
  # expect_equal(dim(data), c(100,8))
  # expect_true("data.frame" %in% class(data))

  data2 <- hugo_read_data(path="example.xlsx")
  expect_equal(dim(data2), c(100,8))
  expect_true("data.frame" %in% class(data2))
})

test_that("Fancy separator % causes error.",{
  expect_error(hugo_read_data("fancy_separator.csv"))
  expect_error(hugo_read_data("fancy_separator.txt"))
})

test_that("Providing own parameters was successfull.",{
  path <- "http://insight.dev.schoolwires.com/HelpAssets/C2Assets/C2Files/C2SectionRobotSample.csv"
  data <- hugo_read_data(path, file_extension = "csv", separator = ",", decimal = ".", header = TRUE)
  expect_equal(dim(data), c(9,4))
  expect_true("data.frame" %in% class(data))
})

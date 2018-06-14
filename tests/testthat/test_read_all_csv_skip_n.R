context("Testing read_all_csv_skip_n")


datasets <- read_all_csv_skip_n(path = "testdata", n = 0)
datasets_2 <- read_all_csv_skip_n(path = "testdata", n = 2)

test_that('read_all_csv_skip_n basic functionality',{


  expect_error(read_all_csv_skip_n(5))
  expect_error(read_all_csv_skip_n("Amy") )
  expect_error(read_all_csv_skip_n("testdata", n = -7))
  expect_error(read_all_csv_skip_n("testdata", n = "Amy"))

  expect_error(read_all_csv_skip_n(path = "folder_path"))
  expect_equal(read_all_csv_skip_n("testdata", n=4), read_all_csv_skip_n("testdata", n = 4.2))

  expect_equal(is.list(datasets), TRUE)
  expect_equal(is.data.frame(datasets[[1]]), TRUE)
  expect_equal(length(datasets),1)

  expect_equal(datasets, read_all_csv_skip_n(path = "testdata", n = 0.2))
  expect_equal(nrow(datasets[[1]]), 80)
  expect_equal(nrow(datasets_2[[1]]), 78)

})

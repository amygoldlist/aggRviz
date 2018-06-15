context("Testing aggR_possible")

load("testdata/yummy.Rda")



sample <- aggR_possible(data = dat_2, features = c("Colour", "Dessert","Fruit", "Sweet_or_Salty"))
sample_n <- aggR_possible(data = dat_2, number = 2, features = c("Colour", "Dessert","Fruit", "Sweet_or_Salty"))


test_that('aggR_possible basic functionality',{

  expect_equal(is.list(sample), TRUE)
  expect_equal("Colour" %in% sample, TRUE)
  expect_equal(sample[[5]], c("Fruit", "Sweet_or_Salty"))
  expect_equal(length(sample), 5)
  expect_equal("Sweet_or_Salty" %in% sample, FALSE)

  expect_equal(is.list(sample_n), TRUE)
  expect_equal("Colour" %in% sample_n, FALSE)
  expect_equal(sample_n[[3]], c("Fruit", "Sweet_or_Salty"))
  expect_equal(length(sample_n), 3)

})



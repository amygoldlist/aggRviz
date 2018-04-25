context("Basic functionality tests")

df_noblanks <- data.frame(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)

df_blanks <- data.frame(
  g1 = c(1, "", 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)




test_that('filter_blanks basic functionality', {
    #outputs
  expect_is(filter_blanks(df_noblanks),'data.frame')
  expect_is(filter_blanks(df_noblanks),'data.frame')
  expect_equal(filter_blanks(df_noblanks), df_noblanks)

  })

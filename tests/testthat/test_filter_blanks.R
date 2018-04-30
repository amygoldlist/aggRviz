context("Basic functionality tests")

df_noblanks <- data.frame(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = c("a", "b", "c", "d", "e"),
  b = c("q","w","e","r","t")
)

df_blanks <- data.frame(
  g1 = c("1", "", "2", "2", "2"),
  g2 = c("1", "2", "1", "2", "1"),
  a = c("a", "b", "c", "d", "e"),
  b = c("q","w","e","","t")
)

df_blanks_filtered <- data.frame(
  g1 = c("1",  "2",  "2"),
  g2 = c("1",  '1',  "1"),
  a = c("a", "c",  "e"),
  b = c("q","e","t")
)



test_that('filter_blanks basic functionality', {
    #outputs
  expect_is(filter_blanks(df_noblanks),'data.frame')
  expect_is(filter_blanks(df_blanks),'data.frame')
  expect_equal(filter_blanks(df_noblanks), df_noblanks)
  expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)
  expect_error(filter_blanks(5), "data should be a dataframe!")
  expect_error(filter_blanks(c(4,65)), "Error: data should be a dataframe!")
  })




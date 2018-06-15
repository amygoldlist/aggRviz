context("tests for join_set")


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

test_that('join_set basic functionality', {
  #outputs
  #expect_is(join_set(df_noblanks, df_blanks),'data.frame')
  #expect_is(filter_blanks(df_blanks),'data.frame')
  #expect_equal(filter_blanks(df_noblanks), df_noblanks)
  #expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)


})


test_that('join_set errors', {

  expect_error(join_set(5,df_blanks), "x should be a dataframe!")
  expect_error(join_set(df_blanks,7), "y should be a dataframe!")
  expect_error(join_set(c(5),df_blanks), "x should be a dataframe!")
  expect_error(join_set(df_blanks,c(7)), "y should be a dataframe!")
})

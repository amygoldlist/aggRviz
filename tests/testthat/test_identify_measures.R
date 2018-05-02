context("Testing identify_measures")


df_blanks <- data.frame(
  measure_1 = c("1", "", "2", "2", "2"),
  measure_2 = c("1", "2", "1", "2", "1"),
  a = c("a", "b", "c", "d", "e"),
  b = c("q","w","e","","t")
)

test_that('identify_measures basic functionality', {
  expect_equal(identify_measures(df_blanks), c("measure_1", "measure_2"))
  #expect_is(filter_blanks(df_noblanks),'data.frame')
  #expect_is(filter_blanks(df_blanks),'data.frame')
  #expect_equal(filter_blanks(df_noblanks), df_noblanks)
  #expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)
  #expect_error(filter_blanks(5), "data should be a dataframe!")
  #expect_error(filter_blanks(c(4,65)), "Error: data should be a dataframe!")
})

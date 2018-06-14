context("Testing identify_measures")


df_blanks <- data.frame(
  measure_1 = c("1", "", "2", "2", "2"),
  measure_2 = c("1", "2", "1", "2", "1"),
  a = c("a", "b", "c", "d", "e"),
  b = c("q","w","e","","t")

)

names_to_check <- c("condition", "odds_ratio", "blood_type", "percent_ratio")

test_that('identify_measures basic functionality', {
  expect_equal(identify_measures(df_blanks), c("measure_1", "measure_2"))
  expect_equal(identify_measures(c("measure_1", "amy", "rate")), c("measure_1", "rate"))
  expect_is(identify_measures(df_blanks),'character')
  expect_equal(identify_measures(5), c())
  expect_equal(identify_measures(names_to_check, key = "ratio"),  c("odds_ratio","percent_ratio"))

  expect_error(identify_measures(y~x), "Error! Data must be a dataframe or vector!")

})



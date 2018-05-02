context("testing aggRviz_join")

load("testdata/yummy.Rda")

df1 <- dat_1 %>%
  dplyr::filter(Dessert == "") %>%
  dplyr::select(-Dessert)

df2 <- dat_2 %>%
  dplyr::filter(Sweet_or_Salty=="") %>%
  dplyr::select(-Sweet_or_Salty)

df <- dplyr::inner_join(df1,df2)


test_that('AggRviz_join basic functionality', {
  expect_equal(aggRviz_join(dat_1,dat_2), df)
  expect_is(aggRviz_join(dat_1,dat_2),'data.frame')
  #expect_is(filter_blanks(df_blanks),'data.frame')
  #expect_equal(filter_blanks(df_noblanks), df_noblanks)
  #expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)
  #expect_error(filter_blanks(5), "data should be a dataframe!")
  #expect_error(filter_blanks(c(4,65)), "Error: data should be a dataframe!")
})


test_that('join_set basic functionality', {
  expect_equal(join_set(dat_1,dat_2), df)
  expect_is(join_set(dat_1,dat_2),'data.frame')
  #expect_is(filter_blanks(df_blanks),'data.frame')
  #expect_equal(filter_blanks(df_noblanks), df_noblanks)
  #expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)
  #expect_error(filter_blanks(5), "data should be a dataframe!")
  #expect_error(filter_blanks(c(4,65)), "Error: data should be a dataframe!")
})

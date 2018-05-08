context("Testing aggR_possible")

load("testdata/yummy.Rda")

##

df1 <- dat_1 %>%
  dplyr::filter(Dessert == "") %>%
  dplyr::select(-Dessert)

df2 <- dat_2 %>%
  dplyr::filter(Sweet_or_Salty=="") %>%
  dplyr::select(-Sweet_or_Salty)

df <- dplyr::inner_join(df1,df2)

skip("skip")

####
test_that('aggR_possible basic functionality', {
  expect_is(aggR_possible(data = dat_2, features = c("Colour", "Dessert","Fruit", "Sweet_or_Salty")), list)
  #expect_is(aggRviz_join(dat_1,dat_2),'data.frame')
  #expect_is(filter_blanks(df_blanks),'data.frame')
  #expect_equal(filter_blanks(df_noblanks), df_noblanks)
  #expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)
  #expect_error(filter_blanks(5), "data should be a dataframe!")
  #expect_error(filter_blanks(c(4,65)), "Error: data should be a dataframe!")
})

###  all the things:
#aggRviz_filter2(dat_1, col_2_keep = c("Colour"))
#aggRviz_filter2(dat_1, col_2_keep = c("Fruit"))
#aggRviz_filter2(dat_1, col_2_keep = c("Dessert"))
#aggRviz_filter2(dat_1, col_2_keep = c("Colour", "Fruit"))
#aggRviz_filter2(dat_1, col_2_keep = c("Colour", "Dessert"))
#aggRviz_filter2(dat_1, col_2_keep = c("Fruit", "Dessert"), features = c("Colour", "Dessert","Fruit", "Sweet_or_Salty"))

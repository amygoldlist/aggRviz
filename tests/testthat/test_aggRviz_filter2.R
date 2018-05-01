
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
  g2 = c("2"),
  a = c("b"),
  b = c("w")
)

df_empty <- dplyr::select(dplyr::filter(df_blanks,g1 == "aggRviz"),b, g2)



test_that('aggRviz_filter2 basic functionality', {

  expect_is(aggRviz_filter2(df_noblanks, col_2_delete=c("g1","a")),'data.frame')
  expect_is(aggRviz_filter2(df_blanks, col_2_delete=c("g2","b")),'data.frame')
  expect_is(aggRviz_filter2(df_noblanks, col_2_delete=c("g1","a")),'data.frame')
  expect_is(aggRviz_filter2(df_blanks, col_2_delete=c("g2","b")),'data.frame')

  expect_is(aggRviz_filter2(df_noblanks, col_2_keep=c("g1","a")),'data.frame')
  expect_is(aggRviz_filter2(df_blanks, col_2_keep=c("g2","b")),'data.frame')
  expect_is(aggRviz_filter2(df_noblanks, col_2_keep=c("g1","a")),'data.frame')
  expect_is(aggRviz_filter2(df_blanks, col_2_keep=c("g2","b")),'data.frame')
  expect_equivalent(aggRviz_filter2(df_blanks, col_2_delete=("g1")), df_blanks_filtered)
  expect_equivalent(aggRviz_filter2(df_blanks, col_2_keep=c("g2", "a", "b")), df_blanks_filtered)
})

test_that('aggRviz_filter errors', {

  expect_error(aggRviz_filter2(5, c("5")), "Error: data should be a dataframe!")
  expect_error(aggRviz_filter2(c(5,6,7), c("5")), "Error: data should be a dataframe!")
  expect_error(aggRviz_filter2(df_blanks, c("5")),"Evaluation error: object '5' not found.")
  expect_error(aggRviz_filter2(df_blanks, df_noblanks),"Error! col_2_delete needs to be a vector!")
  expect_error(aggRviz_filter2(6, df_noblanks),"Error: data should be a dataframe!")

  expect_error(aggRviz_filter2(df_blanks, col_2_delete = c("a"), col_2_keep = c("b")),
               "Error: enter exactly one of col_2_delete and col_2_keep!")
  expect_error(aggRviz_filter2(df_blanks),
               "Error: enter exactly one of col_2_delete and col_2_keep!")
})


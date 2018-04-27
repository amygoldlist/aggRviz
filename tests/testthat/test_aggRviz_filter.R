
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

df_empty <- dplyr::select(dplyr::filter(df_blanks,g1 == "aggRviz"),b, g2)







test_that('aggRviz_filter basic functionality', {

  expect_is(aggRviz_filter(df_noblanks, c("g1","a")),'data.frame')
  expect_is(aggRviz_filter(df_blanks, c("g2","b")),'data.frame')
  expect_error(aggRviz_filter(5, c("5")))
  expect_error(aggRviz_filter(df_blanks, c("5")))
  expect_error(aggRviz_filter(df_blanks, df_noblanks))
  #expect_equivalent(aggRviz_filter(df_noblanks, c("g1","a")), df_empty)
  #expect_equivalent(filter_blanks(df_blanks), df_blanks_filtered)

  })




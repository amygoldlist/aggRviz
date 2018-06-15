
context("Basic functionality tests")
load("testdata/yummy.Rda")

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

#d1 <- data.frame(Dessert = c("cake", "pie"), Colour = c("blue", "red"), measure_1 = c(0.1542357, 0.08528007))

d1 <- dat_1 %>%
  dplyr::filter(Fruit =="" & Colour != "" & Dessert != "") %>%
  dplyr::select(-Fruit) %>%
  dplyr::select(Dessert, dplyr::everything())

d1_rev <- dplyr::select(d1, Colour, dplyr::everything())

#aggRviz_filter2(dat_1, col_2_keep = c("Dessert", "Colour", "measure_1")) == d1


test_that("aggRviz_filter2 on yummy data", {
  expect_equivalent(aggRviz_filter2(dat_1, col_2_keep = c("Dessert", "Colour", "measure_1")), d1_rev )
  expect_equivalent(aggRviz_filter2(dat_1, col_2_delete=c("Fruit")), d1_rev)
  expect_equivalent(aggRviz_filter2(dat_1, col_2_keep =c("Dessert", "Colour"),
                                    features = c("Dessert", "Colour", "Fruit")), d1_rev)
  expect_equivalent(aggRviz_filter2(dat_1, col_2_keep =c("Dessert", "Colour"),
                               features = c("Dessert", "Colour", "Fruit", "Sweet_or_Salty")),d1_rev)
  expect_equivalent(aggRviz_filter2(dat_1, col_2_delete =c("Fruit"), features = c("Dessert", "Colour", "Fruit")), d1_rev)
  expect_equivalent(aggRviz_filter2(dat_1, col_2_delete =c("Fruit"),
                                    features = c("Dessert", "Colour", "Fruit", "Sweet_or_Salty")), d1_rev)
})

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
  #expect_error(aggRviz_filter2(df_blanks, c("5")))  ## this should return correctly now
  expect_error(aggRviz_filter2(df_blanks, df_noblanks),"Error! col_2_delete needs to be a vector!")
  expect_error(aggRviz_filter2(6, df_noblanks),"Error: data should be a dataframe!")

  expect_error(aggRviz_filter2(df_blanks, col_2_delete = c("a"), col_2_keep = c("b")),
               "Error: enter exactly one of col_2_delete and col_2_keep!")
  expect_error(aggRviz_filter2(df_blanks),
               "Error: enter exactly one of col_2_delete and col_2_keep!")
})


test_that('aggRviz_filter2 filtering it all!', {
  df <- dat_1 %>% select(-Colour)
  df2 <- df %>%
    filter(Dessert == "" & Fruit == "") %>%
    select(measure_1)
  expect_equivalent(aggRviz_filter2(data = df,col_2_delete = c("Dessert", "Fruit"),
                                    features = c("Dessert", "Colour", "Fruit")), df2)
  expect_equivalent(aggRviz_filter2(data = df,col_2_keep = "measure_1",
                                    features = c("Dessert", "Colour", "Fruit")),df2)
  expect_equivalent(aggRviz_filter2(data = df,col_2_keep = "measure_1"),df2)

})

context("test of places")


test_that('place checks',{
  dat <- data.frame("Region"= c(1,2,"",""),
                    "Country"= c(1,2,3,""),
                    "State.or.Province" = c(1,"","", ""))
  dat_c <- dat %>%
    dplyr::filter(Country == 3) %>%
    dplyr::select(Country)
  dat_r <- dat %>%
    dplyr::filter(Region == 2) %>%
    dplyr::select(Region)
  dat_s <- dat %>%
    dplyr::filter(State.or.Province == 1) %>%
    dplyr::select(State.or.Province)

  expect_equivalent(aggRviz_filter2(dat, col_2_keep = "Country"), dat_c)
  expect_equivalent(aggRviz_filter2(dat, col_2_keep = "Region"), dat_r)
  expect_equivalent(aggRviz_filter2(dat, col_2_keep = "State.or.Province"), dat_s)
  expect_equivalent(nrow(aggRviz_filter2(dat, col_2_keep = c("Country", "Region"))), 1)
  expect_equivalent(ncol(aggRviz_filter2(dat, col_2_keep = c("Country", "Region"))), 2)
  expect_equivalent(nrow(aggRviz_filter2(dat, col_2_keep = c("Country", "State.or.Province"))), 1)
  expect_equivalent(ncol(aggRviz_filter2(dat, col_2_keep = c("Country", "State.or.Province"))), 2)
  expect_equivalent(nrow(aggRviz_filter2(dat, col_2_keep = c("Region", "State.or.Province"))), 1)
  expect_equivalent(ncol(aggRviz_filter2(dat, col_2_keep = c("Region", "State.or.Province"))), 2)

  ## what if we only have one location?

  #expect_equivalent(aggRviz_filter2(dat, col_2_keep = "Region"), dat_r)
  #expect_equivalent(aggRviz_filter2(dat, col_2_keep = "State.or.Province"), dat_s)

})



test_that('with a missing place',{
  dat <- data.frame(
                    "Country"= c(1,2,3,""),
                    "State.or.Province" = c(1,"","", ""))

  dat_c <- dat %>%
    dplyr::filter(Country %in% c(2,3)) %>%
    dplyr::select(Country)

  dat_s <- dat %>%
    dplyr::filter(State.or.Province == 1) %>%
    dplyr::select(State.or.Province)

  expect_equivalent(aggRviz_filter2(dat, col_2_keep = "Country"), dat_c)
  expect_equivalent(aggRviz_filter2(dat, col_2_keep = "State.or.Province"), dat_s)
  expect_equivalent(nrow(aggRviz_filter2(dat, col_2_keep = c("Country", "Region"))), 2)
  expect_equivalent(ncol(aggRviz_filter2(dat, col_2_keep = c("Country", "Region"))), 1)
  expect_equivalent(nrow(aggRviz_filter2(dat, col_2_keep = c("Country", "State.or.Province"))), 1)
  expect_equivalent(ncol(aggRviz_filter2(dat, col_2_keep = c("Country", "State.or.Province"))), 2)
  expect_equivalent(nrow(aggRviz_filter2(dat, col_2_keep = c("Region", "State.or.Province"))), 1)
  expect_equivalent(ncol(aggRviz_filter2(dat, col_2_keep = c("Region", "State.or.Province"))), 1)



})


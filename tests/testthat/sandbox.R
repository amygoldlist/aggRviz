library(dplyr)
load("example_data/yummy.Rda")

dat_1

dat_2

#newdf <- inner_join(dat_1, dat_2)


### dat_1 is colour, dessert, fruit
### dat_2 is colour Sweet_or Salty, Fruit




df1 <- dat_1 %>%
  filter(Dessert == "") %>%
  select(-Dessert)

df2 <- dat_2 %>%
  filter(Sweet_or_Salty=="") %>%
  select(-Sweet_or_Salty)

df2

df <- inner_join(df1,df2)


df

###OR:

feat_names <- c("Colour", "Sweet_or_Salty", "Fruit", "Dessert")

### add stuff:

library(dplyr)

## read in data
dat <- read.csv("sample_data.csv")


##create datasets

dat_1 <- dat %>%
  select(Colour, Dessert, Fruit, measure_1) %>%
  sample_n(40)

dat_2 <- dat %>%
  select(Colour, Sweet_or_Salty, Fruit, measure_2) %>%
  sample_n(36)


dat_3 <- dat %>%
  select(Dessert, Sweet_or_Salty, Fruit, measure_3) %>%
  sample_n(56)

dat_4 <- dat %>%
  select(Dessert, Colour, measure_4) %>%
  sample_n(43)


write.csv(dat_1, "example_data/measure_1.csv")
write.csv(dat_2, "example_data/measure_2.csv")
write.csv(dat_3, "example_data/measure_3.csv")
write.csv(dat_4, "example_data/measure_4.csv")

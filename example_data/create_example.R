library(dplyr)

## read in data
dat <- read.csv("example_data/sample_data.csv", row.names = NULL)



##create datasets

dat_1 <- dat %>%
  filter(Sweet_or_Salty== "")%>%
  select(Colour, Dessert, Fruit, measure_1) %>%
  sample_n(10)

dat_2 <- dat %>%
  filter(Dessert== "") %>%
  select(Colour, Sweet_or_Salty, Fruit, measure_2) %>%
  sample_n(16)


dat_3 <- dat %>%
  filter(Colour== "") %>%
  select(Dessert, Sweet_or_Salty, Fruit, measure_3) %>%
  sample_n(26)

dat_4 <- dat %>%
  filter(Sweet_or_Salty== "" | Fruit == "") %>%
  select(Dessert, Colour, measure_4) %>%
  sample_n(13)

## save them as datasets:

save(dat_1,dat_2,dat_3, dat_4, file = "example_data/yummy.Rda")


#write.csv(dat_1, "example_data/measure_1.csv")
#write.csv(dat_2, "example_data/measure_2.csv")
#write.csv(dat_3, "example_data/measure_3.csv")
#write.csv(dat_4, "example_data/measure_4.csv")

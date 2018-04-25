df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)

df

df %>% filter_all(all_vars(. != 0))

col <- c("a", "b")

df %>% filter_all(all_vars(col != 0))


df %>% filter_at(col, all_vars((. != 5)))

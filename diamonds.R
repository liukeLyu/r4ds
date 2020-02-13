library(tidyverse)

ggplot(diamonds, mapping=aes(carat, price)) +
  geom_hex()

ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")
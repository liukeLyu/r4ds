library(tidyverse)

as_tibble(iris)

tibble(
  x=1:5,
  y=1,
  z=x^2+y
)

nycflights13::flights %>% 
  print(n = 5, width = 10)

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df$x
df[["x"]]
df[[1]]

df %>% .$x
df %>% .[[1]]

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

(df <- tibble(abc = 1, xyz = "a"))
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying["1"]

annoying %>% 
  ggplot(mapping = aes(x=`1`, y=`2`))+
  geom_point()

annoying %>% 
  mutate(`3` = `2`/`1`) %>% 
  rename(one=`1`, two=`2`, three=`3`)

enframe(1:3)
enframe(c(a=3,b=5))
enframe(list(one = 1, two = 2:3, three = 4:6))
deframe(enframe(1:3))
enframe(1:3)

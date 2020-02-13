library(tidyverse)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  filter(carat < 3) %>% 
  ggplot(mapping = aes(x = carat))+
  geom_histogram(binwidth = 0.1)

diamonds %>% 
  filter(carat < 3) %>% 
  ggplot(mapping = aes(x = carat, color=cut))+
  geom_freqpoly(binwidth = 0.1)

diamonds %>% 
  filter(carat < 3) %>% 
  ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

diamonds %>% 
  ggplot(mapping = aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

(unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y))

diamonds %>% 
  filter(0<x, x<30, 0<y, y<30, 0<z, z<30) %>% 
  ggplot() +
  geom_freqpoly(mapping = aes(x=x), color = "blue",binwidth=0.2)+
  geom_freqpoly(mapping = aes(x=y), color = "red",binwidth=0.2)+
  geom_freqpoly(mapping = aes(x=z), color = "yellow", binwidth=0.2)

diamonds %>% 
  filter(price>1000, price<2000) %>% 
  ggplot(mapping = aes(x=price))+
  geom_histogram(binwidth = 10)

diamonds %>% 
  filter(carat < 1.2, carat > 0.8) %>% 
  ggplot(mapping = aes(x = carat))+
  geom_histogram(binwidth = 0.01)


diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) %>% 
  ggplot(mapping = aes(x=x,y=y))+
  geom_smooth(na.rm=T)+
  geom_point(na.rm=T, alpha=0.1)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds, mapping = aes(x = cut)) + 
  geom_bar()

ggplot(data = diamonds, mapping = aes(x = price, y = ..count..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))+
  coord_flip()

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = cancelled ,y = sched_dep_time)) + 
  geom_boxplot()

diamonds %>% 
  ggplot(mapping = aes(x = carat, y = price))+
  stat_binhex()

diamonds %>% 
  ggplot(mapping = aes(x = cut, y = carat))+
  geom_boxplot()

library(lvplot)
diamonds %>% 
  ggplot(mapping = aes(x = cut, y = price))+
  # geom_boxplot()
  geom_violin(draw_quantiles = c(0.25,0.50,0.75))
  # geom_lv()

diamonds %>% 
  ggplot() +
  geom_count(mapping = aes(x = cut, y = color))
  
diamonds %>% 
  count(color, cut)             

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

#Use geom_tile() together with dplyr to explore 
# how average flight delays vary by destination and month of year. 
# What makes the plot difficult to read? How could you improve it?

library(nycflights13)

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  count(dest, month)

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  count(dest, month) %>% 
  filter(n > 300) %>% 
  ggplot(mapping = aes(x=as.factor(month), y=dest))+
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))

diamonds %>% 
  filter(carat < 3) %>% 
  ggplot(mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

diamonds %>% 
  filter(carat < 3) %>% 
  ggplot(mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

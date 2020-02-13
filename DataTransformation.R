library(nycflights13)
library(tidyverse)

(jan1 <- filter(flights, month==1, day==1))
(nov_dec <- filter(flights, month %in% c(11,12)))
(no_delay <- filter(flights, arr_delay<=120, dep_delay<=120))

# (df <- tibble(x=c(1,NA,3)))
# filter(df, x>1)
# filter(df, is.na(x) | x>1)

filter(flights, arr_delay>=120, dest %in% c("IAH", "HOU"), 
       carrier %in% c("UA","AA","DL"), between(month, 7,9))

filter(flights, dep_delay>=60, dep_delay - arr_delay >= 30)

filter(flights, between(hour,0,6))

filter(flights, is.na(dep_time))

arrange(flights, month, year, day)

arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

ggplot(data=flights)+
  geom_histogram(mapping = aes(x=dep_delay))+
  # geom_histogram(mapping = aes(x=arr_delay))+
  scale_x_continuous(limits=c(-100,300))

fast_flights <- arrange(flights, desc(distance/air_time))

arrange(flights, desc(distance))
arrange(flights, distance)

ggplot(data=select(flights, year, month, day))+
  geom_bar(mapping = aes(x=day))

select(flights, year:day)
select(flights, -(year:day))

select(flights, ends_with("time"))
select(flights, starts_with("time"))
select(flights, contains("time"))
select(flights, contains("time"), contains("hour"))
select(flights, contains("time") & contains("hour"))

select(flights, time_hour, air_time, everything())

# rename(flights, tail_num = tailnum)

select(flights, year, year)

vars <- c("year", "month", "day", "dep_delay", "arr_delay","garbage")
select(flights, vars)
select(flights, one_of(vars))

select(flights, starts_with("dep"), starts_with("arr"))

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

mutate(flights_sml, gain=dep_delay-arr_delay, hours=air_time/60, 
       gain_per_hour = gain / hours)

transmute(flights_sml, gain=dep_delay-arr_delay, hours=air_time/60, 
       gain_per_hour = gain / hours)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

(x <- 1:10)
lag(x)
lead(x)
cumsum(x)
cumprod(x)
cummean(x)

(flights_dep_tm  <-  select(flights, dep_time, sched_dep_time))

mutate(flights_dep_tm,
      dep_minute = (dep_time %/% 100) * 60 + (dep_time %% 100),
      sched_dep_minute = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100)
)

transmute(flights,
          # arr_time,
          # dep_time,
          arr_time_minute = (arr_time %/% 100) * 60 + (arr_time %% 100),
          dep_time_minute = (dep_time %/% 100) * 60 + (dep_time %% 100),
          air_time,
          calculated_air_time = arr_time_minute - dep_time_minute,
          diff = calculated_air_time - air_time
)

min_rank(desc(flights$arr_delay))

# Compare dep_time, sched_dep_time, and dep_delay. 
# How would you expect those three numbers to be related?
transmute(flights,
          dep_time,
          sched_dep_time,
          dep_minute = (dep_time %/% 100) * 60 + (dep_time %% 100),
          sched_dep_minute = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
          dep_delay,
          calculated_dep_delay = dep_minute - sched_dep_minute)
  
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
by_month <- group_by(flights, year, month)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
mean_by_month <- summarise(by_month, delay = mean(dep_delay, na.rm = TRUE))
# mean_by_month <- summarise(by_month, delay = mean(arr_delay, na.rm = TRUE))

ggplot(data=mean_by_month)+
  geom_col(mapping = aes(x=month, y=delay))+
  scale_x_discrete(limits=as.character(1:12))

# Multiple operations
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

# Pipe
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")


# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = T))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data=delays, mapping = aes(x=n, y=delay))+
  geom_point(alpha=0.2)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


# baseball
# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point(alpha=0.2) + 
  geom_smooth(se = F)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

batters %>%
  arrange(desc(ab), desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>% 
  group_by(carrier) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  ) %>%
  arrange(avg_delay2)

# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))
#> # A tibble: 104 x 2
#>   dest  distance_sd
#>   <chr>       <dbl>
#> 1 EGE         10.5 
#> 2 SAN         10.4 
#> 3 SFO         10.2 
#> 4 HNL         10.0 
#> 5 SEA          9.98
#> 6 LAS          9.91
#> # … with 98 more rows

# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>%
  count(dest)

not_cancelled %>%
  count(dest, wt=distance)

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

daily %>% ungroup() %>% summarise(flights = n())

not_cancelled %>% count(dest)

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n=n())

not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% group_by(tailnum) %>% summarise(n = sum(distance))
  
(cancelled_delay<- flights %>% 
                    filter(is.na(arr_delay) & !is.na(dep_delay)) %>% 
                    group_by(year,month,day) %>% 
                    summarise(avg_delay = mean(dep_delay), cancelled=n())
  )

cancelled_delay %>% 
  ggplot(mapping=aes(x=avg_delay, y=cancelled))+
  geom_jitter(alpha=0.2)+
  geom_smooth()

flights %>% 
  group_by(carrier) %>% 
  summarise(
    cancelled = sum(is.na(arr_delay))
  ) %>% 
  ggplot(mapping=aes(x=carrier, y=cancelled))+
  geom_col()


flights %>% 
  filter(is.na(arr_delay)) %>%
  count(carrier, dest) %>% 
  ggplot(mapping=aes(x=carrier, y=n))+
  geom_col()

cancel_rate <- flights %>% 
                group_by(carrier) %>% 
                summarise(cancel_rate = sum(is.na(arr_delay))/n())

cancel_rate %>% 
  ggplot(mapping=aes(x=carrier, y=cancel_rate))+
  geom_col()

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

flights %>%
  filter(!is.na(dep_delay) & !is.na(air_time)) %>% 
  group_by(tailnum) %>% 
  summarise(
    delay_per_airtime = sum(dep_delay) / sum(air_time)
  ) %>% 
  arrange(desc(delay_per_airtime))

flights %>% 
  filter(!is.na(dep_delay) & !is.na(air_time)) %>% 
  group_by(hour) %>% 
  summarise(
    delay_per_airtime = sum(dep_delay) / sum(air_time)
  ) %>% 
  ggplot(mapping = aes(x=hour, y=delay_per_airtime))+
  geom_col()

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(dest) %>% 
  summarise(
    delay_total = sum(dep_delay)
  )

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(dest) %>% 
  transmute(
    tot_delay = sum(dep_delay),
    prop_delay = dep_delay / tot_delay
  )

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(year,month,day,hour) %>% 
  select(year,month,day,hour,dep_delay) %>% 
  arrange(hour) %>% 
  mutate(dep_delay_lag = lag(dep_delay)) %>% 
  filter(!is.na(dep_delay_lag)) %>% 
  ggplot(mapping = aes(x=dep_delay, y=dep_delay_lag))+
  stat_binhex(bins=50)+
  geom_smooth(alpha=0.2)


flights %>% 
  arrange(air_time)
  # select(dest, air_time) %>% 
flights %>% 
  group_by(dest) %>% 
  mutate(
    relative_air_time = air_time / min(air_time)
  ) %>% 
  arrange(desc(relative_air_time))


flights %>% 
  group_by(dest) %>% 
  summarise(n_carriers = n_distinct(carrier)) %>% 
  filter(n_carriers>=2) %>%
  arrange(desc(n_carriers))

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(tailnum) %>% 
  summarise(
    min(rank(time_hour[dep_delay > 60]))
  )
  # mutate(r = min_rank(time_hour))

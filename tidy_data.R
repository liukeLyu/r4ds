library(tidyverse)

# table4a <- table4a %>%
#   pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
# 
# table4b <- table4b %>%
#   pivot_longer(c(`1999`,`2000`), names_to = "year", values_to = "population")

left_join(table4a, table4b)

left_join(
table2 %>% 
  filter(type == "cases") %>% 
  mutate(cases = count) %>% 
  select(-c(count, type)),
table2 %>% 
  filter(type == "population") %>% 
  mutate(population = count) %>% 
  select(-c(count, type))
)

table2 %>% 
  pivot_wider(names_from = type, values_from = count)

(stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
))

stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return", 
               names_ptypes = list(year=double()))

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

(people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
))

people %>% 
  pivot_wider(names_from = names, values_from = values)

(preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
))

preg %>% 
  pivot_longer(c(male,female), names_to = "gender", values_to = "count")

table3

table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep="/", convert=T)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

table5 %>% 
  unite(year, century, year, sep="") %>% type_convert()

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "drop", remove=F)

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill="right")


# Missing Values
(stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
))

stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "year", 
    values_to = "return", 
    values_drop_na = TRUE
  )

stocks %>% 
  complete(year, qtr)


(treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
))
treatment %>% fill(person)

who_clean <- who %>% 
  pivot_longer(cols = new_sp_m014:newrel_f65,
               names_to = "group", 
               values_to = "cases",
               values_drop_na = T) %>% 
  mutate(group = str_replace(group, "newrel", "new_rel")) %>% 
  separate(col = group, into = c("new","type","sexage"),sep = "_") %>% 
  separate(col = sexage, into = c("sex","age"), sep = 1) %>% 
  select(-iso2, -iso3, -new)

who_clean %>% 
  group_by(country, year, sex) %>% 
  count() %>% 
  ggplot(mapping = aes(x = year, y=n, color=sex)) +
  geom_col(position = "stack")

# Prove that iso2 and iso3 are redundant, given country
who %>% 
  pivot_longer(cols = new_sp_m014:newrel_f65,
               names_to = "group", 
               values_to = "cases",
               values_drop_na = T) %>% 
  group_by(country) %>% 
  summarise(
    distincts = n_distinct(iso3)
  ) %>% 
  filter(distincts > 1)

who_clean %>% 
  summarise(n_distinct(country))

who_clean %>% 
  summarise(n_distinct(type))

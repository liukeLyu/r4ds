library(tidyverse)

heights <- read_csv("data/heights.csv")

read_csv("a,b,c
1,2,3
4,5,6")

read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# a comment I want to skip
         x,y,z
         1,2,3
         4,5,6", comment="#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("name", "age", "sex"))
read_csv("a,b,c\n1,2,.", na = ".")

read_delim("1|2|3\n4|5|6", delim = "|", col_names = F)

read_csv("x,y\n1,'a,b'", col_names = F, quote = "'")

read_csv("a,b,c\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2,.\n1,2,3,4")
read_csv("a,b\n\"1,.")
read_csv("a,b\n1,2\na,b")
read_csv2("a;b\n1;3")

str(parse_logical(c("TRUE", "FALSE", "NA")))
parse_logical(c("TRUE", "FALSE", "NA"))

parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))

parse_number("1,23", locale = locale(decimal_mark = ","))
parse_number("$100")
parse_number("20%")
parse_number("It Cost $123.45")
parse_number("$123,456,789")
parse_number("$123.456.789,123", locale = locale(grouping_mark = ".", decimal_mark = ","))

fruit <- c("apple", "banana","bananana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")
parse_date("2010-10-10")

parse_time("01:10 am")
parse_time("21:10")
parse_date("01/02/15", "%m/%d/%y")
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

parse_date("January 1, 2010", "%B %d, %Y")
parse_date("2015-Mar-07", "%Y-%b-%d")
parse_date("06-Jun-2017", "%d-%b-%Y")
parse_date(c("August 19 (2015)", "July 1 (2015)"), "%B %d (%Y)")
parse_date("12/30/14", "%m/%d/%y")
parse_date("1705","%y%m")

guess_parser("11:15:10.12 PM")
parse_time("11:15:10.12 PM")

# How to read a file
guess_parser("2010-10-01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1","2","3"))
guess_parser(c("1","223,321","3"))

challenge <- read_csv(readr_example("challenge.csv"))

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
)

df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
type_convert(df)

write_csv(challenge, "challenge.csv")
read_csv("challenge.csv")

write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot()
#>  NULL
library(magrittr)
rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot()
#>  num [1:50, 1:2] -0.387 -0.785 -1.057 -0.796 -1.756 ...

mtcars %$%
  cor(disp, mpg)



#11.3.5 Question 7
library(readr)

d1 <- "January 1, 2010"

parse_date(d1, "%B %d, %Y")


d2 <- "2015-Mar-07"

parse_date(d2, "%Y-%b-%d")

d3 <- "06-Jun-2017"

parse_date(d3, "%d-%b-%Y")

d4 <- c("August 19 (2015)", "July 1 (2015)")

parse_date(d4, "%B %d (%Y)")

d5 <- "12/30/14" # Dec 30, 2014

parse_date(d5, "%m/%d/%y")

t1 <- "1705"

parse_time(t1, "%H%M")

t2 <- "11:15:10.12 PM"

parse_time(t2, "%H:%M:%OS %p")

#exercise 12.6.1
#In this case study, I set na.rm = TRUE just to make it easier to check that we had the correct values. 
#Is this reasonable? Think about how missing values are represented in this dataset. 
#Are there implicit missing values? What’s the difference between an NA and zero?
who
who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )
who1

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,  '' , 0.92, 0.17, 2.66)
)
stocks
stocks %>% 
  pivot_wider(names_from = year, values_from = return)
stocks %>% 
  pivot_wider(names_from = qtr, values_from = return)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(c(`2015`,`2016`), names_to = "year", values_to = "return", values_drop_na = TRUE)


who1_dropNA <- who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = "key",
  values_to = "cases",
  values_drop_na = TRUE
)
nrow(who1_dropNA)

who1_keepNA <- who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = "key",
  values_to = "cases"
)
who1_keepNA
nrow(who1_keepNA)

### how to verify?
## step 1.check for the presence of zeros in the data.
who1 %>%
  filter(cases == 0) %>%
  nrow()
# observation: There are zeros in the data

## step 2.check whether all values for a (country, year) are missing or 
# whether it is possible for only some columns to be missing
result <- who1_keepNA %>% 
  group_by(country, year) %>% 
  mutate(missing = sum(is.na(cases)), total = n(),missing_percentage = sum(is.na(cases)) / n()) %>%
  filter(missing_percentage > 0, missing_percentage < 1)
view(result)         
who1_keepNA %>% 
  group_by(country, year) %>% 
  summarise(n=n())

who1_keepNA %>% 
  group_by(country, year) %>% 
  mutate(missing = sum(is.na(cases)), total = n(),missing_percentage = sum(is.na(cases)) / n()) %>% 
  filter(country == "Afghanistan", year >= 2006)
view(who1_keepNA)
# observation: it looks like it is possible for a (country, year) row to 
# contain some, but not all, missing values in its columns

## step 3.check for implicit missing values
# Implicit missing values are (year, country) combinations that do not appear in the data.
nrow(who)
who %>% 
  complete(country,year) %>% 
  nrow()
anti_join(complete(who, country, year), who, by = c("country", "year"))%>% 
  select(country, year) %>% 
  group_by(country) %>%  
  summarise(min_year = min(year), max_year = max(year))

who

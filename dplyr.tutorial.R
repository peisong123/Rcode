install.packages("dplyr")
install.packages("hflights")
library(dplyr)
library(hflights)
head(hflights)
str(hflights)

flights <- tbl_df(hflights)
flights
print(flights, n=100)

flights11=flights[flights$Month==1 & flights$DayofMonth==1, ]
flights11=filter(flights, Month==1, DayofMonth==1)
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")
filter(flights, UniqueCarrier %in% c("AA", "UA"))

# base R approach to select DepTime, ArrTime, and FlightNum columns
flights[ ,c("DepTime", "ArrTime", "FlightNum")]
select(flights, DepTime, ArrTime, FlightNum)

# use colon to select multiple contiguous columns, and use `contains` to match columns by name
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

# nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

# chaining method
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)


# base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelay
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

# dplyr approach
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)

# base R approach to create a new variable Speed (in mph)
flights$Speed <- flights$Distance / flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]

# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

# store the new variable
flights <- flights %>% mutate(Speed = Distance/AirTime*60)

# dplyr approach: create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

# for each carrier, calculate the percentage of flights cancelled or diverted
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

# for each carrier, calculate the minimum and maximum arrival and departure delays
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("Delay"))


# for each day of the year, count the total number of flights and sort in descending order
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# rewrite more simply with the `tally` function
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

# for each destination, count the total number of flights and the number of distinct planes that flew there
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

# for each carrier, calculate which two days of the year they had their longest departure delays
# note: smallest (not largest) value is ranked as 1, so you have to use `desc` to rank by largest value
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# rewrite more simply with the `top_n` function
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# randomly sample a fixed number of rows, without replacement
flights %>% sample_n(5)
# randomly sample a fraction of rows, with replacement
flights %>% sample_frac(0.25, replace=TRUE)

# dplyr approach: better formatting, and adapts to your screen width
glimpse(flights)

# connect to an SQLite database containing the hflights data
my_db <- src_sqlite("my_db.sqlite3")

install.packages("RSQLite")
install.packages("DBI")
library(RSQLite)
library(DBI)

# installs everything you need to use sqldf with SQLite
# including SQLite itself
install.packages("sqldf")
# shows built in data frames
data() 
# load sqldf into workspace
library(sqldf)









install.packages('tidyverse')
install.packages('lubridate')
library(tidyverse)
library(lubridate)

q1_2019 <- read.csv('Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv')
q1_2020 <- read.csv('Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv')

### Tasks for data wrangling ###
# 1 - Make columns consistent between the 2 data sets
# 2 - Combine data sets
# 3 - Align the columns between the 2 data sets and remove inconsistent columns if necessary
# ==========================================================================================

q1_2019 <- q1_2019 %>%
  rename(
    ride_id = trip_id,
    rideable_type = bikeid,
    started_at = start_time,
    ended_at = end_time,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    member_casual = usertype
  )

# convert ride_id and rideable_type to character to allow for combining data sets
q1_2019 <- q1_2019 %>%
  mutate(ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type))

all_trips = bind_rows(q1_2019, q1_2020)
rm(q1_2019, q1_2020)

# remove columns that only exist in one data set but not both
columns_to_remove = c("tripduration", "gender", "birthyear", "start_lat", "start_lng", "end_lat", "end_lng")
all_trips <- all_trips %>%
  select(-all_of(columns_to_remove))


### Tasks for data cleaning ###
# 1 - Align naming of member/subscriber and casual/customer between the data sets
# 2 - Add more columns for date to help with grouping during the analysis
# 3 - Add a ride length column (tripduration was removed from the 2019 data set)
# 4 - Remove columns with 'bad' data - Rides starting at HQ or rides with a negative ride length
# ==============================================================================================

# make the member_casual column values consistent
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

# add columns, for date, day, month, year, and ride_length
all_trips <- all_trips %>%
  mutate(date = as_date(started_at),
         day = format(date, "%d"),
         month = format(date, "%m"),
         year = format(date, "%y"),
         day_of_week = format(date, "%A"),
         ride_length = as.numeric(difftime(ended_at, started_at, units="mins"))
  )

# remove rows with a negative ride length or rides started at HQ
all_trips_v2 <- all_trips[(all_trips$start_station_name != "HQ QR" & all_trips$ride_length > 0),]
rm(all_trips)
install.packages('ggplot2')
library(ggplot2)

# summary statistics
summary(all_trips_v2$ride_length)

# members take short rides compared to casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=min)

# analyze rider type by day of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", "Saturday")
                                    )

all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarize(mean(ride_length)) # big jump in average ride lenght for casual riders on Thursdays and Fridays

# analyze rider type by day of the week
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% # casual riders take more rides on the weekends, members take more rides during the weekdays
  # visualize
  ggplot(aes(x=day_of_week, y=number_of_rides, fill=member_casual)) +
  geom_col(position='dodge') +
  labs(title="Number of Rides by Day of the Week", subtitle="Ridership data from Q1 2019 and Q1 2020",
       x="Day of the Week", y="Number of Rides", fill="Rider Type") +
  theme(axis.text.x = element_text(angle=45))

# visualize the difference in average ride length
all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  ggplot(aes(x=day_of_week, y=average_duration, fill=member_casual)) +
  geom_col(position='dodge') +
  labs(title="Average Ride Length by Day of the Week", subtitle="Ridership data from Q1 2019 and Q1 2020",
       x="Day of the Week", y="Average Ride Length (minutes)", fill="Rider Type") +
  theme(axis.text.x = element_text(angle=45))

## Introduction
#Welcome to the Cyclistic bike-share analysis case study! In this case study,
#I performed many real-world tasks of a junior data analyst for a fictional company, Cyclistic.

## Scenario

#Cyclistic, is a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. 
#Therefore, our team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, our team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve the recommendations, 
#so they must be backed up with compelling data insights and professional data visualizations.

rm(list=ls())

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/shash/OneDrive/Documents")

# Upload Divvy datasets (csv files) here
q2_2019 <- read_csv("q2_2019.csv")
q3_2019 <- read_csv("q3_2019.csv")
q4_2019 <- read_csv("q4_2019.csv")
q1_2020 <- read_csv("q1_2020.csv")

# Rename columns  to make them consistent with q1_2020 
#as this will be the supposed going-forward table design for Divvy
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))



# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)


# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
 select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

  
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)


# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year
#before completing these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at,"%d-%m-%Y") #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$start <- format(as.POSIXct(all_trips$started_at),"%H:%M:%S")
all_trips$ended <- format(as.POSIXct(all_trips$ended_at),"%H:%M:%S")


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


#Remove bad data
# The dataframe includes a few hundred entries when bikes were taken out of docks 
#and checked for quality by Divvy or ride_length was negative
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
head(all_trips_v2)

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride



# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)


# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#the days of the week are out of order. Let's fix that
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#visualizing the number of rides by rider type
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(ride_count = length(ride_id)) %>%
  ggplot() + geom_col(mapping = aes(x = member_casual, y = ride_count, fill = member_casual), show.legend = FALSE) +
  labs(title = "Total no. of rides ")

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	


#visualizing the number of rides by rider type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# visualization for average duration of rider type by day of the week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Visualization of Average ride length every month by rider type 
all_trips_v2 %>%
  mutate(month=month(started_at,label=TRUE))%>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = "drop") %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length vs. month")

#Visualization of mean distance travelled by rider type
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(average_ride_distance = mean(ride_length)) %>%
  ggplot() + geom_col(mapping = aes(x = member_casual, y = average_ride_distance, fill = member_casual), show.legend = FALSE) +
  labs(title = "Mean distance travelled")









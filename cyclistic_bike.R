##load the necessary library needed for project 
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)
library(geosphere)
library("DescTools") 


##load all downloaded csv files into respective dataframes

Dec_2021 <-read_csv("202112-divvy-tripdata.csv")
Jan_2022 <-read_csv("202201-divvy-tripdata.csv")
Feb_2022 <-read_csv("202202-divvy-tripdata.csv")
Mar_2022 <-read_csv("202203-divvy-tripdata.csv")
Apr_2022 <-read_csv("202204-divvy-tripdata.csv")
May_2022 <-read_csv("202205-divvy-tripdata.csv")
Jun_2022 <-read_csv("202206-divvy-tripdata.csv")
Jul_2022 <-read_csv("202207-divvy-tripdata.csv")
Aug_2022 <-read_csv("202208-divvy-tripdata.csv")
Sep_2022 <-read_csv("202209-divvy-tripdata.csv")
Oct_2022 <-read_csv("202210-divvy-tripdata.csv")
Nov_2022 <-read_csv("202211-divvy-tripdata.csv")


#merge all dataframes into one
cyclistic <- rbind(Dec_2021, Jan_2022, Feb_2022, Mar_2022, Apr_2022, May_2022, Jun_2022, Jul_2022, Aug_2022, Sep_2022, Oct_2022, Nov_2022)

#add new columns for data cleaning and analysis
new_cyclistic <- cyclistic%>%
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units="mins" )))%>%
  mutate(ride_distance = distHaversine(cbind (start_lng, start_lat), cbind(end_lng, end_lat))) %>%
  mutate(ride_year = year(started_at))%>%
  mutate(ride_month = month(started_at))%>%
  mutate(day_of_week = weekdays(started_at))%>%
  mutate(hour_of_day = hour(started_at))

##filter out rows without valuesclike start station, end station, zero distanceand rides from 5 mins

clean_cyclistic <-new_cyclistic %>%
  filter(!is.na(start_station_name))%>%
  filter(!is.na(end_station_name))%>%
  filter(ride_length >= 5)%>%
  filter(ride_distance>0)

##removing unwanted columns such as start_lng, start_lat, end_lng and end_lat

clean_cyclistic <- clean_cyclistic%>%
  select(-c(start_lng, start_lat, end_lng, end_lat))

#summarize and group our table for data visualization

viz_cyclistic <- clean_cyclistic%>%
  group_by(member_casual, rideable_type, ride_year, ride_month, day_of_week, hour_of_day)%>%
  summarize(number_of_ride=n(), avg_ride_length=mean(ride_length), avg_ride_distance=mean(ride_distance))


##write viz_cyclistic to csv file
write.csv(viz_cyclistic, 'vizcyclistic.csv')
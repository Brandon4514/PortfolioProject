# Cyclist Bike-Share:Case Study
Project for the Google Data Analytics Professional Certificate

## Intoduction and Scenario
You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

### About the company
 In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

## Phase 1: Ask

The director of marketing and manager Lily Moreno has set a clear goal to design marketing strategies aimed at converting casual riders into annual members.I'm assisgned with crating insights to answer the following questions:

Three questions will guide the future marketing program:
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

## Phase 2: Prepare
The data that we will be using is Cyclist's historical trip data from the last 12 months in this case (July-2021 to June-2022). The data has been made available by Motivate Internation Inc.(https://divvy-tripdata.s3.amazonaws.com/index.html)under this licnese(https://ride.divvybikes.com/data-license-agreement)

The data is organized by 12 CSV files (each for a month) with 13 columns and more than 5 million rows

In order to protect the privacy of the customers, any personal information such as credit number and adresses are being omitted. 

The ROCCC approach is used to determine the crediblity of the data
* Reliable – It is complete and accurate and it represents all bike rides taken in the city of Chicago for the selected duration of our analysis.
* Original - The data is made available by Motivate International Inc. which operates the city of Chicago’s Divvy bicycle sharing service which is powered by Lyft.
* Comprehensive - the data includes all information about ride details including starting time, ending time, station name, station ID, type of membership and many more.
* Current – It is up-to-date as it includes data until end of June 2022
* Cited - The data is cited and is available under Data License Agreement.

## Phase 3: Process
The tools that I will be using for the process phase is R Programming. R is very useful to handle huge datasets efficiently.

#### Install and load the necessary packages
```r
install.packages("tidyverse")
install.packages("skimr")
install.packages("lubridate")
install.packages("janitor")
installed.packages("scales")
install.packages("data.table")
install.packages("dplyr")
install.packages("hms")
library(hms)
library(tidyverse)
library(skimr)
library(lubridate)
library(janitor)
library(scales)
library(readr)
library(data.table)
library(dplyr)
```
#### insert all 12 csv files and merge them into one whole dataset
```r
#load the data frames
jul07_df <- read_csv("202106-divvy-tripdata.csv")
aug08_df <- read_csv("202107-divvy-tripdata.csv")
sep09_df <- read_csv("202108-divvy-tripdata.csv")
oct10_df <- read_csv("202109-divvy-tripdata.csv")
nov11_df <- read_csv("202110-divvy-tripdata.csv")
dec12_df <- read_csv("202111-divvy-tripdata.csv")
jan01_df <- read_csv("202112-divvy-tripdata.csv")
feb02_df <- read_csv("202201-divvy-tripdata.csv")
mar03_df <- read_csv("202202-divvy-tripdata.csv")
apr04_df <- read_csv("202203-divvy-tripdata.csv")
may05_df <- read_csv("202204-divvy-tripdata.csv")
jun06_df <- read_csv("202205-divvy-tripdata.csv")

#merge the data frames into one
cyclist_df <- rbind( jul07_df, aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df)

```
#### Cleaning 
```r
#remove all the nulls
clean_cyclist <- drop_na(cyclist_df)

# remove unnecessary columns  
clean_cyclist = subset(clean_cyclist, select = -c(start_lat,start_lng,end_lat, end_lng))

#remove duplicates 
clean_cyclist <- distinct(clean_cyclist)
```
#### Prepare for analysis
* Added column called "ride_length and calculate the length of each ride 
* Added new columns to calculate the following for each ride
  * Date
  * Year
  * Month
  * Day
  * Day of week
```r
# add a column that calculates the ride length in minutes  
clean_cyclist$ride_lenght <- difftime(clean_cyclist$ended_at, clean_cyclist$started_at, units = "mins")
clean_cyclist$ride_lenght <- as.numeric(as.character(clean_cyclist$ride_lenght))

#add a new column for day of the week ( 1= Monday and 7= Sunday )
clean_cyclist$date <- as.Date(clean_cyclist$started_at) # start of the day 
clean_cyclist$day_of_week <- wday(clean_cyclist$started_at) # day of the week
clean_cyclist$day_of_week <- format(as.Date(clean_cyclist$date), "%A") # day of the week column 
clean_cyclist$month <- format(as.Date(clean_cyclist$date), "%m") # month column 
clean_cyclist$day <- format(as.Date(clean_cyclist$date), "%d") # day column 
clean_cyclist$year <- format(as.Date(clean_cyclist$date), "%y") # year column 
clean_cyclist$time <- format(as.Date(clean_cyclist$date), "%H:%M:%S")
clean_cyclist$time <- as_hms((clean_cyclist$started_at))
clean_cyclist$hour <- hour(clean_cyclist$time) # column for time
```
#### check for errors 
Use the filter to get rid of rows where trips where negative duration 
```r
#remove where ride_length is less than a minute
clean_cyclist <- clean_cyclist[!(clean_cyclist$ride_lenght <= 1),]
```
## Phase 4: Aanlyze

Performed data aggregation using R Programming
```r
filter(clean_cyclist, ride_lenght > 870000 & member_casual == "casual")
clean_cyclist <- clean_cyclist[!(clean_cyclist$ride_lenght > 870000 & clean_cyclist$member_casual =="casual"),]

#difference between casual and members 
aggregate(clean_cyclist$ride_lenght ~ clean_cyclist$member_casual, FUN = mean)
aggregate(clean_cyclist$ride_lenght ~ clean_cyclist$member_casual, FUN = median)
aggregate(clean_cyclist$ride_lenght ~ clean_cyclist$member_casual, FUN = max)
aggregate(clean_cyclist$ride_lenght ~ clean_cyclist$member_casual, FUN = min)

#average ride time per day casual vs member 
aggregate(clean_cyclist$ride_lenght ~ clean_cyclist$member_casual + clean_cyclist$day_of_week, FUN = mean)

# create new data frame for members vs casuals by weekday ,average duration, numbers of rides and ride type
member_casual_ridetype <- clean_cyclist %>%
  group_by(member_casual, day_of_week, rideable_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_lenght)) %>%
  arrange(member_casual, day_of_week)

```
## Phase 5: Share
![Number_of_rides_by_hour](https://user-images.githubusercontent.com/108597401/177033124-f1bd038b-1e1f-40dd-a81f-7f3635f601e0.png)

In this Graph we will see the number of rides broken down into the hours of the day. For both of the groups we could see that the rides are rising throughout the day beginning from 4 to 17 hours being the peak for both of them. But for members we notoce that we have three peaks one being 8 hours during the moening ,the other one  12 hours during the lunch time and the last one being at 17 hours in the evening. 

![Number_rides_by_days_into_hours](https://user-images.githubusercontent.com/108597401/177031731-0d425387-aca1-4d8e-a5e1-525aa5908944.png)

In these graphs we could observe that the rides are broken down into each day of the week by hours.

![Number_of_rides_by_type](https://user-images.githubusercontent.com/108597401/177031938-715ece9d-441a-4809-8065-5307e4e9ea5b.png)

In this Graph we could see that member riders are more active through the week days, while casual riders are more active in the weekends

![Number_of_rides_month](https://user-images.githubusercontent.com/108597401/177032549-03ce65b4-b57f-46ad-80f2-013ef7d49946.png)

In this graph the number of rides are broken into the months of the year. As we could see through the months of May to October the rides tend to be at its highest. which could be understanable because they are warmer months. While in the colder months such as November to March rides tend to be at its lowest

## Phase 6: Act
After analyzing, we reached into the following conclusion:
* Casual riders tend to be more active during the weekends and member throughout the week days.
* During the months of May to Septmber are the most active on both groups
* casual riders tend to use bikes more for recreational purposes

My top 3 recommendations based on my analysis 
1. Design packages system where casual riders are incentivize to buy weekends pass. Also add additional perks to members where after a ceratin amount of miles they get discounts on their next memberships or if they refer a friend.
2. Design some type of packages system where casual riders are able to buy seasonal pass for specific periods of the year
3. Create effective and efficient promotions for members on busiest times and station. this will incetivize casual riders to buy member pass in order to access bike during busy times.



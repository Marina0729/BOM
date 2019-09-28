.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)

read_csv("data/BOM_data.csv")
BOM_data <- read_csv("Data/BOM_data.csv")

read_csv("data/BOM_stations.csv")
BOM_stations <- read_csv("Data/BOM_stations.csv")


BOM_data

BOM_stations

#Challenge 1
#Question 1: For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

#Steps taken 
#1--Separate Temp_min_max into two separate columns 
#2--Remove rows in Temp_min and Temp_max containing "-" 
#3--Remove rows in Rainfall containing "-"
#4--Create a "Date" column since I don't know if more than one measurement was taken in one day
#5--Don't need Solar_exposure column
#6--Take existing tbl and reduce it to one row per station
#7--summarise creating new colum "days_with_full_measurements" equals the
#   number of unique values in the "Date" column

Challenge1_0 <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  select(-Solar_exposure) %>%
  unite(Year, Month, Day, col = "Date", sep = "-") %>%
  group_by(Station_number) %>% 
  summarise(days_with_full_measurements = n_distinct(Date))

write_csv(Challenge1_0,"Results/Challenge1_0.csv") 

#Try it using group by Date instead of summarise number of distinct elements

Challenge1_1 <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  select(-Solar_exposure) %>%
  unite(Year, Month, Day, col = "Date", sep = "-") %>%
  group_by(Date) %>% 
  group_by(Station_number) %>% 
  count(Station_number, Station_number)

write_csv(Challenge1_1,"Results/Challenge1_1.csv") 

#What would happen if I just reported the number of rows for each station
#without making sure there was only one measurement taken per day? 

Challenge1_2 <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  select(-Solar_exposure) %>% 
  group_by(Station_number) %>% 
  count(Station_number, Station_number)
  
#Same answer! This suggests there is one row per day. Let's test this using
#group_by(Date) and see how many observations there are in the tibble.

Check_number_unique_Dates <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  select(-Solar_exposure) %>%
  unite(Year, Month, Day, col = "Date", sep = "-") %>%
  group_by(Date)

#There are 28340 obs. in this tibble

Check_number_rows_after_filtering <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  group_by(Day)

#There are 28340 obs. in this tibble

#Lets check station 86344 is a tibble 1,020 obs.

Check_station_86344 <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  select(-Solar_exposure) %>% 
  filter(Station_number == 86344) %>% 
  arrange(Year, Month, Day)

#Yes

#Challenge 2
#Which month saw the lowest average daily temperature difference?

BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max !="-") %>% 
  filter(Rainfall != 0) %>% 
  group_by(Month) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% 
  arrange(Mean_temp_diff)

#Answer = 6!

#Challenge 3
#Which state saw the lowest average daily temperature difference?
Tidy_BOM_data <- BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max !="-") %>% 
  filter(Rainfall != 0) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  select(Station_number, Temp_diff) %>% 
  mutate(Station_number = as.numeric(Station_number))

Tidy_BOM_data

State_BOM_Stations <- BOM_stations %>%
  gather(Station_number, value, -info) %>% 
  filter(info == "state")

inner_join(State_BOM_Stations, Tidy_BOM_data, by = "Station_number")

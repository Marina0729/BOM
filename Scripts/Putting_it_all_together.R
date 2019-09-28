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

write_csv(Challenge1_2,"Results/Challenge1_2.csv") 
  
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
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>% 
  select(-Solar_exposure) %>%
  group_by(Month) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% 
  arrange(Mean_temp_diff)

#Answer = 6, or June



#Challenge 3
#Which state saw the lowest average daily temperature difference?

Station_number_Mean_temp_diff <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  select(-Rainfall) %>% 
  select(-Solar_exposure) %>%
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  group_by(Station_number) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% 
  arrange(Station_number)
  
BOM_stations

Station_number_state <- 
  BOM_stations %>% 
  filter(info == "state") %>% 
  gather(Station_number, state, 2:21) %>% 
  select(-info) %>% 
  mutate(Station_number_numeric = as.numeric(Station_number)) %>% 
  select(-Station_number)

names(Station_number_Mean_temp_diff)[1]<-"Station_number_numeric"

Challenge3 <- 
  full_join(Station_number_state, Station_number_Mean_temp_diff, by = "Station_number_numeric") %>%
  group_by(state) %>% 
  summarise(Mean_temp_diff = mean(Mean_temp_diff)) %>% 
  arrange(Mean_temp_diff)
  
write_csv(Challenge3,"Results/Challenge3.csv") 

#Answer is Victoria!

#Challenge 4
#Does the westmost (lowest longitude) or eastmost (highest longitude) 
#weather station in our dataset have a higher average solar exposure?

Station_number_Mean_Solar_exposure <- 
  BOM_data %>% 
  select(Station_number, Solar_exposure) %>% 
  filter(Solar_exposure != "-") %>% 
  mutate(Solar_exposure_numeric = as.numeric(Solar_exposure)) %>% 
  group_by(Station_number) %>% 
  summarise(Mean_Solar_exposure = mean(Solar_exposure_numeric))

Station_number_lon <- 
  BOM_stations %>% 
  filter(info == "lon") %>% 
  gather(Station_number, lon, 2:21) %>% 
  select(-info) %>% 
  mutate(Station_number_numeric = as.numeric(Station_number)) %>% 
  select(-Station_number)

names(Station_number_Mean_Solar_exposure)[1]<-"Station_number_numeric"

Challenge4 <- 
  inner_join(Station_number_lon, Station_number_Mean_Solar_exposure, by = "Station_number_numeric") %>%
  arrange(lon, Mean_Solar_exposure)

View(Challenge4)

#Answer is eastmost!
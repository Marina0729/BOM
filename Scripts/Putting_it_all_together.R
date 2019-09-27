.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)

read_csv("data/BOM_data.csv")
BOM_data <- read_csv("data/BOM_data.csv")

read_csv("data/BOM_stations.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")


BOM_data

BOM_stations


#Challenge 1
#Question 1: For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

 
Challenge1 <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max !="-") %>% 
  filter(Rainfall != 0) %>%
  select(-Solar_exposure) %>% 
  mutate(date_identifier = Year + Month + Day) %>% 
  arrange(date_identifier) %>% 
  summarise(n_days = n_distinct(date_identifier))


#answer is 200



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

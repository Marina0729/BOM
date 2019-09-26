.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)

read_csv("data/BOM_data.csv")
BOM_data <- read_csv("data/BOM_data.csv")

read_csv("data/BOM_stations.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")


BOM_data

BOM_stations


#Challenge 1
##Question 1: For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

 
BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max !="-") %>% 
  filter(Rainfall != 0) %>% 
  group_by(Station_number) %>% 
  summarise(n = n())




  



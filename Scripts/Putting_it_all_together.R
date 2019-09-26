.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)

read_csv("data/BOM_data.csv")
BOM_data <- read_csv("data/BOM_data.csv")

read_csv("data/BOM_stations.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")


BOM_data

BOM_stations

Seperated_Temp <- BOM_data %>% 
  separate(BOM_data, col = Temp_min_max, into = c("Temp_min", "Temp_Max")) %>% 
  filter(Temp_min_max == "-/-") %>% 
  group_by() %>% 
  summarise() %>% 
 


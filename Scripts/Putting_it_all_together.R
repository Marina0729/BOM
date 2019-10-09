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
  filter(Rainfall != "-")
  

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

#Create a data frame with new column Temp_diff
BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>% 
  select(-Solar_exposure) %>%
  select(-Rainfall) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
#summarise to Mean_temp_diff by Month
  group_by(Month) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% 
  arrange(Mean_temp_diff)

#Answer = 6, or June



#Challenge 3
#Which state saw the lowest average daily temperature difference?

#First get Mean_temp_diff for each station number 
Station_number_Mean_temp_diff <- 
  BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  select(-Rainfall) %>% 
  select(-Solar_exposure) %>%
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  group_by(Station_number) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff))
  
  
BOM_stations
#Want to extract station number and state from this data frame. 
#This data frame is too wide, want to "gather" to fewer columns. 
#Also need to create a new column with numeric station number values

Station_number_state <- 
  BOM_stations %>% 
  filter(info == "state") %>% 
  gather(Station_number, state, 2:21) %>% 
  select(-info) %>% 
  mutate(Station_number_numeric = as.numeric(Station_number)) %>% 
  select(-Station_number)

#Try using a gather and spread method from Stephen 
Station_number_state2 <- 
  BOM_stations %>% 
  filter(info == "state") %>% 
  gather(key = "Station_number", value = "state", -info) %>% 
  select(-info) %>% 
  mutate(Station_number_numeric = as.numeric(Station_number)) %>% 
  select(-Station_number)



#Station_number_Mean_temp_diff data frame needs same column name to join

names(Station_number_Mean_temp_diff)[1]<-"Station_number_numeric"

#Join the two data frames Station_number_numeric then group by state and
#summarise 
Challenge3 <- 
  full_join(Station_number_state, Station_number_Mean_temp_diff, by = "Station_number_numeric") %>%
  group_by(state) %>% 
  summarise(Mean_temp_diff = mean(Mean_temp_diff)) %>% 
  arrange(Mean_temp_diff)

Challenge3_2 <- 
  left_join(Station_number_state2, Station_number_Mean_temp_diff, by = "Station_number_numeric") %>%
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

#Answer is eastmost, by a whistle!


#Below is Stephen's answer to Q3
# Q3: Which state saw the lowest average daily temperature difference? -----

# State information does not exist in the BOM_data.csv file. It is in the BOM_stations.csv file. We
# will need to join these two data frames together to use them. But BOM_stations.csv has
# the station identifiers as column headers, while BOM_data.csv has them as the values in a column.
# We will need to tidy BOM_stations.csv with a gather and spread before we can join it.

# We want to take the column names and store them in a new column called "Station_number" to match 
# how they are named in the BOM_data data frame. We will create a new column called "values" to store 
# all the information that used to be stored in the columns under each station number. By default we 
# would gather up the data from *all* columns doing this, but we want to do it for everything but the 
# info column, so we can exclude that with '-info'

stations_very_long <- BOM_stations %>% 
  gather(key = "Station_number", value = "values", -info) 

# Eg of structure:
# A tibble: 140 x 3
# info  Station_number values                
# <chr> <chr>          <chr>                 
#  elev  9194           14                    
#  end   9194           2018                  
#  lat   9194           -32.2208           

# We now want to restructure the data frame again. This time creating new columns with names from the 
# current info column and the contents of the new columns coming from the current 'values' column
# This is the reverse of a gather step and so is a spread

stations_tidy <- stations_very_long %>% 
  spread(key = info, value = values)

# Eg. of structure:
# A tibble: 20 x 8
# Station_number elev  end   lat      lon      name                              start state
# <chr>          <chr> <chr> <chr>    <chr>    <chr>                             <chr> <chr>
#  14825          88.5  2018  -16.403  131.0145 VICTORIA RIVER DOWNS              1885  NT   
#  14909          416   2018  -13.3275 133.0861 CENTRAL ARNHEM PLATEAU            2003  NT   
#  22050          41.1  2018  -33.9703 137.6628 KADINA AWS                        2005  SA   

# Try to join the two together now. Both have a "Station_number" column to join on
# But this line gives an error:

#BOM_combined <- left_join(BOM_data, stations_tidy)
#
#Error: Can't join on 'Station_number' x 'Station_number' because of incompatible types (character / numeric)

# So we need to convert them to the same data type. A slightly dangerous method is to just overwrite
# the column in the stations_tidy data frame. It's unlikely to be an issue in this case as we are
# never going to need the station numbers as a character

stations_tidy <- mutate(stations_tidy, Station_number = as.numeric(Station_number))

#Join the two together properly now. Both have a "Station_number" column to join on
#This brings the station metadata (including state) into our data frame with the meterological measurements
BOM_with_temps <- BOM_data %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") 
BOM_combined <- left_join(BOM_with_temps, stations_tidy)

# Now can run the same analysis as for Q2. Only differences are that we will group by state instead
# of month
q3_ans <- BOM_combined %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%
  filter(!is.na(t_diff)) %>% 
  group_by(state) %>% 
  summarise(avg_t_diff = mean(t_diff)) %>% 
  arrange(avg_t_diff)

#Print it to the screen if running in RStudio
#Can see that the QLD has the lowest average differences between min and max temperatures (7.36)
q3_ans


write_csv(q3_ans, "results/q3_avg_tempdiff_by_state.csv")

#Question 1
#For the Perth Station (9225), produce three scatter plots showing the relationship 
#between the maximun temperature and each other measurement recorded 
#(min temp, rainfall and solar exposure)

tidy_data_q1 <- BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep ="/") %>% 
  filter(Temp_min != "-") %>% 
  filter(Temp_max != "-") %>% 
  filter(Rainfall != "-") %>%
  filter(Solar_exposure !="-") %>% 
  select(-Year, -Month, -Day) %>% 
  filter(Station_number == "9225") %>% 
  mutate(Temp_min = as.numeric(Temp_min), Temp_max = as.numeric(Temp_max), Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure))


plot_Temp_min <- ggplot(data = tidy_data_q1,
                mapping = aes(x = Temp_max, y = Temp_min)) + geom_point(alpha = 0.2)
plot_Rainfall <- ggplot(data = tidy_data_q1,
                mapping = aes(x = Temp_max, y = Rainfall)) + geom_point(alpha = 0.2) 
plot_Solar_exposure <- ggplot(data = tidy_data_q1,
                mapping = aes(x = Temp_max, y = Solar_exposure)) + geom_point(alpha = 0.2)

install.packages("cowplot")
.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(cowplot)


q1_plot <- plot_grid(plot_Temp_min, plot_Rainfall, plot_Solar_exposure)

ggsave(filename = "Results/q1.png", plot = q1_plot, width = 12, height = 10, dpi = 300, units = "cm")

#Question 2
#Display these four measurements for the Perth station in one plot using
#additonal aesthetic mappings 


q2_plot <- ggplot(data = tidy_data_q1) +
  geom_point( mapping = aes(x = Temp_max, 
                            y = Temp_min, 
                            colour = Solar_exposure, 
                            alpha = Rainfall)) +
  theme(legend.text = element_text(size = 2), legend.title = element_text(size = 2))



ggsave(filename = "Results/q2.png", plot = q2_plot, width = 15, height = 10, dpi = 600, units = "cm")

#Question 3
#Take four plots and save them as a figure 


q3_plot <- plot_grid(plot_Temp_min, plot_Rainfall, 
                     plot_Solar_exposure, q2_plot, 
                     rel_heights = c(1, 3))

ggsave(filename = "Results/q3.png", plot = q3_plot, width = 15, height = 10, dpi = 600, units = "cm")

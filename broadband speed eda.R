library(tidyverse)
library(plotly)

#Cleaned data sets
bristolBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-broadband-speed.csv")
View(bristolBroadbandSpeed)
cornwallBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/cornwall-broadband-speed.csv")
View(cornwallBroadbandSpeed)
bcBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-cornwall-broadband-speed.csv")
View(bcBroadbandSpeed)

options(scipen = 1000)

#BOX PLOT: AVERAGE DOWNLOAD SPEED IN BRISTOL AND CORNWALL COUNTY
BroadbandSpeedBoxPlot = bcBroadbandSpeed %>% 
  #Column "County" is added for easier plotting
  mutate(County = case_when(
    `postcode area` == "BS" ~ "CITY OF BRISTOL",
    `postcode area` %in% c("PL", "TR") ~ "CORNWALL")) %>% 
  ggplot(aes(x = County, y = `Average download speed (Mbit/s)`)) + 
  geom_boxplot() +
  labs(title = "Average Download Speed in Counties of Bristol and Cornwall",
       x = "County",
       y = "Average Download Speed") +
  theme_minimal()
ggplotly(BroadbandSpeedBoxPlot, tooltip = "text")

#BAR CHARTS: AVERAGE AND MAXIMUM DOWNLOAD SPEEDS
#In the city of bristol there are more than 200 lsoa areas
  #and if they all were to be plotted, the bar chart would look congested
nrow(bristolBroadbandSpeed %>% 
       filter(city == "Bristol, City of") %>% 
       group_by(lsoa_area) %>% 
       summarise())

#So a sample of the data set is used
#FOR LSOA AREAS OF BRISTOL
bristolBroadbandSpeed %>%
  filter(city == "Bristol, City of") %>%
  group_by(lsoa_area) %>%
  #The average and maximum speed of each lsoa area is calculated
  summarise(
    avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
    max_speed = max(`Maximum download speed (Mbit/s)`, na.rm = TRUE)
  ) %>%
  #20 random lsoa areas are selected
  slice_sample(n = 20) %>%
  #pivot_longer: combines the two columns into one column "speed" and adds a new column for type of speed
  pivot_longer(cols = c(avg_speed, max_speed), names_to = "speed_type", values_to = "speed") %>%
  ggplot(aes(x = lsoa_area, y = speed, fill = speed_type)) +
  #position = dodge used to create grouped bar chart
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average and Maximum Download Speeds across LSOA areas of Bristol city",
    x = "LSOA Area",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  #to make the lsoa area texts to appear vertically for neatness
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

cornwallBroadbandSpeed %>%
  filter(city == "Cornwall") %>%
  group_by(lsoa_area) %>%
  summarise(
    avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
    max_speed = max(`Maximum download speed (Mbit/s)`, na.rm = TRUE)
  ) %>%
  slice_sample(n = 20) %>%
  pivot_longer(cols = c(avg_speed, max_speed), names_to = "speed_type", values_to = "speed") %>%
  ggplot(aes(x = lsoa_area, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average and Maximum Download Speeds across LSOA areas of Cornwall city",
    x = "LSOA Area",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
     

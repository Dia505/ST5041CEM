library(tidyverse)
library(lubridate)
library(plotly)

#loading of cleaned data sets
bristolHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-house-pricing.csv")
View(bristolHousePrice)
cornwallHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/cornwall-house-pricing.csv")
View(cornwallHousePrice)
bcHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-cornwall-house-pricing.csv")
View(bcHousePrice)

#BOX PLOT: HOUSE PRICES OF 2022 IN COUNTIES BRISTOL AND CORNWALL
#options(scipen = 1000): removes scientific numbers in the axes
options(scipen = 1000)

HousePrice22BoxPlot = bcHousePrice %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  ggplot(aes(x = County, y = Price)) + 
  geom_boxplot() +
  labs(title = "House Prices of 2022 in Bristol and Cornwall",
       x = "County",
       y = "House Price") +
  #y-axis limits are set to focus on house prices between 150000 and 800000
  coord_cartesian(ylim = c(150000, 800000)) + 
  theme_minimal()
#ggplotly + tooltip = "text" are used to make the label appear only when hovering over the plot
  #It gives a cleaner look
ggplotly(HousePrice22BoxPlot, tooltip = "text")

#The plot shows that the min, q1, median, q3 and upper fence values are very close together
  #making the boxes very narrow
  #and this has been caused due to an extreme max outlier which is, on average, 
  #44.635 times greater than the upper fence
#The summaries below of counties Bristol and Cornwall in 2022 illustrate this situation 
summary(bcHousePrice %>% 
          filter(County == "CITY OF BRISTOL",
                 year(Date_of_transfer) == 2022))

summary(bcHousePrice %>% 
          filter(County == "CORNWALL",
                 year(Date_of_transfer) == 2022))

#BAR CHART: AVERAGE HOUSE PRICES OF 2022 DIFFERENTIATED BY VARIOUS PROPERTIES
property_type_labels = c(
  "D" = "Detached",
  "F" = "Flat",
  "O" = "Other",
  "S" = "Semi-Detached",
  "T" = "Terraced"
)

#FOR THE TOWNS OR CITIES OF BRISTOL COUNTY
bristolHousePrice %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  group_by(`Town/City`, Property_type) %>% 
  summarise(avg_price = mean(Price)) %>% 
  ggplot(aes(x = Property_type, y = avg_price, fill = Property_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~`Town/City`) +
  #To create appendix of property types
  scale_fill_discrete(labels = property_type_labels, 
                      name = "Property Type") +
  labs(title = "Average House Prices of 2022 in Towns/Cities of Bristol",
       x = "",
       y = "Average House Price",
       fill = property_type_labels) 

#FOR THE TOWNS OR CITIES OF CORNWALL COUNTY
cornwallHousePrice %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  group_by(`Town/City`, Property_type) %>% 
  summarise(avg_price = mean(Price)) %>% 
  ggplot(aes(x = Property_type, y = avg_price, fill = Property_type)) +
  geom_bar(stat = "identity", width = 0.4) +
  facet_wrap(~`Town/City`) +
  scale_fill_discrete(labels = property_type_labels, 
                      name = "Property Type") +
  labs(title = "Average House Prices of 2022 in Towns/Cities of Cornwall",
       x = "",
       y = "Average House Price",
       fill = property_type_labels) +
  #To adjust size of facet labels
  theme(strip.text = element_text(size = 6),  
        #To adjust size of y-axis labels
        axis.text.y = element_text(size = 7))

#LINE PLOT: AVERAGE HOUSE PRICES FROM 2020 TO 2023 IN BRISTOL AND CORNWALL COUNTIES
bcHousePrice %>% 
  mutate(Year = year(Date_of_transfer)) %>% 
  group_by(Year, County) %>% 
  summarise(avg_price = mean(Price)) %>% 
  ggplot(aes(x = Year, y = avg_price)) +
  geom_line() +
  geom_point() + 
  labs(title = "Line Plot: Average House Prices from 2020-2023",
       x = "Year",
       y = "Average House Price") +
  facet_wrap(~County) 

  

library(tidyverse)
library(lubridate)

#loading of cleaned data sets
bristolHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-house-pricing.csv")
View(bristolHousePrice)
cornwallHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/cornwall-house-pricing.csv")
View(cornwallHousePrice)
bcHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-cornwall-house-pricing.csv")

bristolBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-broadband-speed.csv")
View(bristolBroadbandSpeed)
cornwallBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/cornwall-broadband-speed.csv")
View(cornwallBroadbandSpeed)
bcBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-cornwall-broadband-speed.csv")

bs_housePrice_broadBand_2022 = inner_join(
  bristolHousePrice, 
  bristolBroadbandSpeed,
  by = c("Postcode" = "postcode_space")) %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  select(Price, `Average download speed (Mbit/s)`)
View(bs_housePrice_broadBand_2022)
nrow(bs_housePrice_broadBand_2022)

#Weak, negative relationship (-0.0131)
bs_housePrice_broadBand_2022 %>% 
  summarise(corCoeff = cor(Price, `Average download speed (Mbit/s)`))

model_housePrice_broadBand = lm(Price ~ `Average download speed (Mbit/s)`, data = bs_housePrice_broadBand_2022)
summary(model_housePrice_broadBand)

intercept_Price_DownloadSpeed = coef(model_housePrice_broadBand)[1]
slope_Price_DownloadSpeed = coef(model_housePrice_broadBand)[2]

options(scipen = 1000)

#A scatter plot is created to visualize the relationship of house price and average download speed
ggplot(bs_housePrice_broadBand_2022, aes(x = `Average download speed (Mbit/s)`, y = Price)) +
  #The points of the plot are coloured blue and have size 2
  geom_point(color = "blue", size = 2) +   
  #A red line of best fit is created using calculated values of intercept and slope
  #geom_abline() is a function used to add a straight line to the plot
  geom_abline(intercept = intercept_Price_DownloadSpeed, slope = slope_Price_DownloadSpeed, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on House Price in Bristol (2022)",
    x = "Average Downloadd Speed",
    y = "House Price"
  ) +
  theme_minimal()

cw_housePrice_broadBand_2022 = inner_join(
  cornwallHousePrice, 
  cornwallBroadbandSpeed,
  by = c("Postcode" = "postcode_space")) %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  select(Price, `Average download speed (Mbit/s)`)
View(cw_housePrice_broadBand_2022)
nrow(cw_housePrice_broadBand_2022)

#Weak, negative relationship (-0.0194)
cw_housePrice_broadBand_2022 %>% 
  summarise(corCoeff = cor(Price, `Average download speed (Mbit/s)`))

model_housePrice_broadBand_cw = lm(Price ~ `Average download speed (Mbit/s)`, data = cw_housePrice_broadBand_2022)
summary(model_housePrice_broadBand_cw)

intercept_Price_DownloadSpeed_cw = coef(model_housePrice_broadBand_cw)[1]
slope_Price_DownloadSpeed_cw = coef(model_housePrice_broadBand_cw)[2]

ggplot(cw_housePrice_broadBand_2022, aes(x = `Average download speed (Mbit/s)`, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = intercept_Price_DownloadSpeed_cw, slope = slope_Price_DownloadSpeed_cw, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on House Price in Cornwall (2022)",
    x = "Average Downloadd Speed",
    y = "House Price"
  ) +
  theme_minimal()

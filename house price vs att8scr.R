library(tidyverse)
library(lubridate)

bristolHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-house-pricing.csv")
View(bristolHousePrice)
cornwallHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/cornwall-house-pricing.csv")

bristolSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/bristol-schools.csv")
View(bristolSchools)
cornwallSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/cornwall-schools.csv")

bristolHousePrice_2022_2023 = bristolHousePrice %>% 
  filter(year(Date_of_transfer) == 2022 | year(Date_of_transfer) == 2023) %>% 
  group_by(Postcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop')
View(bristolHousePrice_2022_2023)

cornwallHousePrice_2022_2023 = cornwallHousePrice %>% 
  filter(year(Date_of_transfer) == 2022 | year(Date_of_transfer) == 2023) %>% 
  group_by(Postcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop')
View(cornwallHousePrice_2022_2023)

bristolSchools_2022_2023 = bristolSchools %>% 
  filter(Year == 2022 | Year == 2023) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(POSTCODE) %>%
  summarise(Avg_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')
View(bristolSchools_2022_2023)

cornwallSchools_2022_2023 = cornwallSchools %>% 
  filter(Year == 2022 | Year == 2023) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(POSTCODE) %>%
  summarise(Avg_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')
View(cornwallSchools_2022_2023)

#LINEAR MODELLING
#FOR BRISTOL
bs_housePrice_att8scr_2022_2023 = inner_join(bristolHousePrice_2022_2023, bristolSchools_2022_2023, 
                                             by = c("Postcode" = "POSTCODE")) %>% 
  select(Avg_Price, Avg_ATT8SCR)
View(bs_housePrice_att8scr_2022_2023)

#Weak, negative relationship (-0.291)
bs_housePrice_att8scr_2022_2023 %>% 
  summarise(corCoeff = cor(Avg_Price, Avg_ATT8SCR))

bsModel_housePrice_att8scr_2022_2023 = lm(Avg_Price ~ Avg_ATT8SCR, 
                                          data = bs_housePrice_att8scr_2022_2023)
summary(bsModel_housePrice_att8scr_2022_2023)

bsIntercept_housePrice_att8scr_2022_2023 = coef(bsModel_housePrice_att8scr_2022_2023)[1]
bsSlope_housePrice_att8scr_2022_2023 = coef(bsModel_housePrice_att8scr_2022_2023)[2]

options(scipen = 1000)

ggplot(bsModel_housePrice_att8scr_2022_2023, aes(x = Avg_ATT8SCR, y = Avg_Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = bsIntercept_housePrice_att8scr_2022_2023, slope = bsSlope_housePrice_att8scr_2022_2023, color = "red") +      
  labs(
    title = "Influence of Attainment 8 Scores on House Price in Bristol (2022-2023)",
    x = "Attainment 8 Scores (average)",
    y = "House Price (average)"
  ) +
  theme_minimal()

#FOR CORNWALL
cw_housePrice_att8scr_2022_2023 = inner_join(cornwallHousePrice_2022_2023, cornwallSchools_2022_2023, 
                                             by = c("Postcode" = "POSTCODE")) %>% 
  select(Avg_Price, Avg_ATT8SCR)
View(cw_housePrice_att8scr_2022_2023)

#Strong, negative relationship (-1)
cw_housePrice_att8scr_2022_2023 %>% 
  summarise(corCoeff = cor(Avg_Price, Avg_ATT8SCR))

cwModel_housePrice_att8scr_2022_2023 = lm(Avg_Price ~ Avg_ATT8SCR, 
                                          data = cw_housePrice_att8scr_2022_2023)
summary(cwModel_housePrice_att8scr_2022_2023)

cwIntercept_housePrice_att8scr_2022_2023 = coef(cwModel_housePrice_att8scr_2022_2023)[1]
cwSlope_housePrice_att8scr_2022_2023 = coef(cwModel_housePrice_att8scr_2022_2023)[2]

ggplot(cwModel_housePrice_att8scr_2022_2023, aes(x = Avg_ATT8SCR, y = Avg_Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = cwIntercept_housePrice_att8scr_2022_2023, slope = cwSlope_housePrice_att8scr_2022_2023, color = "red") +      
  labs(
    title = "Influence of Attainment 8 Scores on House Price in Cornwall (2022-2023)",
    x = "Attainment 8 Scores (average)",
    y = "House Price (average)"
  ) +
  theme_minimal()

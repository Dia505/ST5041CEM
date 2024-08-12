library(tidyverse)
library(lubridate)

bristolHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-house-pricing.csv")
cornwallHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/cornwall-house-pricing.csv")

bristolSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/bristol-schools.csv")
View(bristolSchools)
cornwallSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/cornwall-schools.csv")

bristolHousePrice2022 = bristolHousePrice %>% 
  filter(year(Date_of_transfer) == 2022)
View(bristolHousePrice2022)
cornwallHousePrice2022 = cornwallHousePrice %>% 
  filter(year(Date_of_transfer) == 2022)

bristolSchools2022 = bristolSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))
View(bristolSchools2022)

cornwallSchools2022 = cornwallSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))
View(cornwallSchools2022)

#bristolHousePrice2022 %>%
  #group_by(Postcode) %>%
  #filter(n() > 1)

#bristolSchools2022 %>%
  #group_by(POSTCODE) %>%
  #filter(n() > 1)

#bristolHousePrice2022 = bristolHousePrice2022 %>%
  #group_by(Postcode) %>%
  #summarise(
    #avgPrice = mean(Price, na.rm = TRUE),
    #.groups = 'drop')

bs_housePrice_att8scr_2022 = inner_join(bristolHousePrice2022, bristolSchools2022, by = c("Postcode" = "POSTCODE")) %>% 
  select(Price, ATT8SCR)
View(bs_housePrice_att8scr_2022)
dim(bs_housePrice_att8scr_2022)


library(tidyverse)
library(lubridate)

bristolHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-house-pricing.csv")
View(bristolHousePrice)
cornwallHousePrice = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/cornwall-house-pricing.csv")

bristolCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/bristol-crime-rate.csv")
View(bristolCrimeRate)
cornwallCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/cornwall-crime-rate.csv")
View(cornwallCrimeRate)

bristolHousePrice2022 = bristolHousePrice %>% 
  filter(year(Date_of_transfer) == 2022)
View(bristolHousePrice2022)
dim(bristolHousePrice2022)

cornwallHousePrice2022 = cornwallHousePrice %>% 
  filter(year(Date_of_transfer) == 2022)
View(cornwallHousePrice2022)
dim(cornwallHousePrice2022)

#Data set of population from 2011 according to postcodes
population_2011 = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/population/Population2011_1656567141570.csv")
View(population_2011)
dim(population_2011)
colSums(is.na(population_2011))

#Population of 2011 is converted to that of 2023
population_2023 = population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)
View(population_2023)

#Conversion of population of 2023 to 2022
years_between = 2023-2011
annual_growth_rate = 1.00561255390388033 ^ (1/years_between)
population_2022 = population_2023 %>% 
  mutate(Population = Population/annual_growth_rate)
View(population_2022)
dim(population_2022)

str(bristolCrimeRate)

bristolCrimeRate = bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(bristolCrimeRate)
str(bristolCrimeRate)

cornwallCrimeRate = cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(cornwallCrimeRate)

bs_drugOffenceRate_2022 = bristolCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of") 

View(bs_drugOffenceRate_2022)
colSums(is.na(bs_drugOffenceRate_2022))
dim(bs_drugOffenceRate_2022)

#bs_drugOffenceRate_2022 is refined by removing rows with NA population
bs_drugOffenceRate_2022 = bs_drugOffenceRate_2022 %>% 
  filter(!is.na(Population))
dim(bs_drugOffenceRate_2022)

bs_drugOffenceRate_2022 = bs_drugOffenceRate_2022 %>% 
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)
View(bs_drugOffenceRate_2022)

cw_drugOffenceRate_2022 = cornwallCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Cornwall") 
View(cw_drugOffenceRate_2022)
dim(cw_drugOffenceRate_2022)

cw_drugOffenceRate_2022 = cw_drugOffenceRate_2022 %>% 
  filter(!is.na(Population))
dim(cw_drugOffenceRate_2022)

cw_drugOffenceRate_2022 = cw_drugOffenceRate_2022 %>% 
  group_by(postcode_space) %>% 
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)
View(cw_drugOffenceRate_2022)

#LINEAR MODELLING
#FOR BRISTOL
bs_housePrice_drugRate_2022 = inner_join(
  bristolHousePrice2022, bs_drugOffenceRate_2022,
  by = c("Postcode" = "postcode_space")) %>% 
  select(Price, drug_offense_rate)
View(bs_housePrice_drugRate_2022)

#Weak, negative relationship (-0.0260)
bs_housePrice_drugRate_2022 %>% 
  summarise(corCoeff = cor(Price, drug_offense_rate))

bsModel_housePrice_drugRate = lm(Price ~ drug_offense_rate ,data = bs_housePrice_drugRate_2022)
summary(bsModel_housePrice_drugRate)

bsIntercept_housePrice_drugRate = coef(bsModel_housePrice_drugRate)[1]
bsSlope_housePrice_drugRate = coef(bsModel_housePrice_drugRate)[2]

options(scipen = 1000)

ggplot(bs_housePrice_drugRate_2022, aes(x = drug_offense_rate, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = bsIntercept_housePrice_drugRate, slope = bsSlope_housePrice_drugRate, color = "red") +      
  labs(
    title = "Impact of Drug Offence on House Price in Bristol (2022)",
    x = "Drug Offence Rate",
    y = "House Price"
  ) +
  theme_minimal()

#FOR CORNWALL
cw_housePrice_drugRate_2022 = inner_join(
  cornwallHousePrice2022, cw_drugOffenceRate_2022,
  by = c("Postcode" = "postcode_space")) %>% 
  select(Price, drug_offense_rate)
View(cw_housePrice_drugRate_2022)

#Weak, negative relationship (-0.0483)
cw_housePrice_drugRate_2022 %>% 
  summarise(corCoeff = cor(Price, drug_offense_rate))

cwModel_housePrice_drugRate = lm(Price ~ drug_offense_rate ,data = cw_housePrice_drugRate_2022)
summary(cwModel_housePrice_drugRate)

cwIntercept_housePrice_drugRate = coef(cwModel_housePrice_drugRate)[1]
cwSlope_housePrice_drugRate = coef(cwModel_housePrice_drugRate)[2]

ggplot(cw_housePrice_drugRate_2022, aes(x = drug_offense_rate, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = cwIntercept_housePrice_drugRate, slope = cwSlope_housePrice_drugRate, color = "red") +      
  labs(
    title = "Impact of Drug Offence on House Price in Cornwall (2022)",
    x = "Drug Offence Rate",
    y = "House Price"
  ) +
  theme_minimal()

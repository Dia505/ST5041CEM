library(tidyverse)

bristolBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-broadband-speed.csv")
View(bristolBroadbandSpeed)
cornwallBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/cornwall-broadband-speed.csv")
View(cornwallBroadbandSpeed)

bristolCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/bristol-crime-rate.csv")
View(bristolCrimeRate)
cornwallCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/cornwall-crime-rate.csv")
View(cornwallCrimeRate)

#Data set of population from 2011 according to postcodes
population_2011 = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/population/Population2011_1656567141570.csv")
View(population_2011)

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

bristolCrimeRate = bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(bristolCrimeRate)

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

#bs_drugOffenceRate_2022 is refined by removing rows with NA population
bs_drugOffenceRate_2022 = bs_drugOffenceRate_2022 %>% 
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)
View(bs_drugOffenceRate_2022)
dim(bs_drugOffenceRate_2022)

cw_drugOffenceRate_2022 = cornwallCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Cornwall") 
View(cw_drugOffenceRate_2022)
dim(cw_drugOffenceRate_2022)

cw_drugOffenceRate_2022 = cw_drugOffenceRate_2022 %>% 
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>% 
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)
dim(cw_drugOffenceRate_2022)
View(cw_drugOffenceRate_2022)

#LINEAR MODELLING
#FOR BRISTOL
bs_drugRate_downloadSpeed_2022 = inner_join(
  bristolBroadbandSpeed, bs_drugOffenceRate_2022,
  by = "postcode_space") %>% 
  select(`Average download speed (Mbit/s)`, drug_offense_rate)
View(bs_drugRate_downloadSpeed_2022)

#Weak negative relationship (-0.271)
bs_drugRate_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(drug_offense_rate, `Average download speed (Mbit/s)`))

bsModel_drugRate_downloadSpeed = lm(drug_offense_rate ~ `Average download speed (Mbit/s)`, data = bs_drugRate_downloadSpeed_2022)
summary(bsModel_drugRate_downloadSpeed)

bsIntercept_drugRate_downloadSpeed = coef(bsModel_drugRate_downloadSpeed)[1]
bsSlope_drugRate_downloadSpeed = coef(bsModel_drugRate_downloadSpeed)[2]

options(scipen = 1000)

ggplot(bs_drugRate_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = drug_offense_rate)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = bsIntercept_drugRate_downloadSpeed, slope = bsSlope_drugRate_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Internet Speed on Drug Offense Rates in Bristol (2022)",
    x = "Average Download Speed",
    y = "Drug Offence Rate"
  ) +
  theme_minimal()

#FOR CORNWALL
cw_drugRate_downloadSpeed_2022 = inner_join(
  cornwallBroadbandSpeed, cw_drugOffenceRate_2022,
  by = "postcode_space") %>% 
  select(`Average download speed (Mbit/s)`, drug_offense_rate)
View(cw_drugRate_downloadSpeed_2022)

#Weak negative relationship (-0.142)
cw_drugRate_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(drug_offense_rate, `Average download speed (Mbit/s)`))

cwModel_drugRate_downloadSpeed = lm(drug_offense_rate ~ `Average download speed (Mbit/s)`, data = cw_drugRate_downloadSpeed_2022)
summary(cwModel_drugRate_downloadSpeed)

cwIntercept_drugRate_downloadSpeed = coef(cwModel_drugRate_downloadSpeed)[1]
cwSlope_drugRate_downloadSpeed = coef(cwModel_drugRate_downloadSpeed)[2]

ggplot(cw_drugRate_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = drug_offense_rate)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = cwIntercept_drugRate_downloadSpeed, slope = cwSlope_drugRate_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Internet Speed on Drug Offense Rates in Cornwall (2022)",
    x = "Average Download Speed",
    y = "Drug Offence Rate"
  ) +
  theme_minimal()

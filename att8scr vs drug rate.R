library(tidyverse)

bristolCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/bristol-crime-rate.csv")
View(bristolCrimeRate)
cornwallCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/cornwall-crime-rate.csv")
View(cornwallCrimeRate)

bristolSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/bristol-schools.csv")
View(bristolSchools)
cornwallSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/cornwall-schools.csv")
View(cornwallSchools)

bristolSchools_2022_2023 = bristolSchools %>% 
  filter(Year == 2022 | Year == 2023) 
View(bristolSchools_2022_2023)
colSums(is.na(bristolSchools_2022_2023))

bristolSchools_2022_2023 = bristolSchools_2022_2023 %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

cornwallSchools_2022_2023 = cornwallSchools %>% 
  filter(Year == 2022 | Year == 2023)
View(cornwallSchools_2022_2023)
colSums(is.na(cornwallSchools_2022_2023))

cornwallSchools_2022_2023 = cornwallSchools_2022_2023 %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

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

population_2022_2023 = left_join(population_2022, population_2023, by = "Postcode") %>%
  rename(population_2022 = Population.x, population_2023 = Population.y) %>%
  mutate(avg_population = (population_2022 + population_2023) / 2)
View(population_2022_2023)

bristolCrimeRate = bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(bristolCrimeRate)

cornwallCrimeRate = cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(cornwallCrimeRate)

bs_drugOffenceRate_2022_2023 = bristolCrimeRate %>% 
  filter(year(Year) %in% c(2022, 2023)) %>% 
  left_join(population_2022_2023, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of") %>% 
  group_by(postcode_space) %>%
  summarise(drug_offences = n(), avg_population = first(avg_population)) %>%
  mutate(drug_offence_rate = drug_offences / avg_population * 10000)

View(bs_drugOffenceRate_2022_2023)
dim(bs_drugOffenceRate_2022_2023)
colSums(is.na(bs_drugOffenceRate_2022_2023))

bs_drugOffenceRate_2022_2023 = bs_drugOffenceRate_2022_2023 %>% 
  filter(!is.na(avg_population))

cw_drugOffenceRate_2022_2023 = cornwallCrimeRate %>% 
  filter(year(Year) %in% c(2022, 2023)) %>% 
  left_join(population_2022_2023, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Cornwall") %>% 
  group_by(postcode_space) %>%
  summarise(drug_offences = n(), avg_population = first(avg_population)) %>%
  mutate(drug_offence_rate = drug_offences / avg_population * 10000)

View(cw_drugOffenceRate_2022_2023)
dim(cw_drugOffenceRate_2022_2023)
colSums(is.na(cw_drugOffenceRate_2022_2023))

cw_drugOffenceRate_2022_2023 = cw_drugOffenceRate_2022_2023 %>% 
  filter(!is.na(avg_population))

#LINEAR MODELLING
#FOR BRISTOL
bs_att8scr_drugRate_2022_2023 = inner_join(
  bristolSchools_2022_2023, bs_drugOffenceRate_2022_2023,
  by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, drug_offence_rate)
View(bs_att8scr_drugRate_2022_2023)

#FOR CORNWALL
cw_att8scr_drugRate_2022_2023 = inner_join(
  cornwallSchools_2022_2023, cw_drugOffenceRate_2022_2023,
  by = c("POSTCODE" = "postcode_space")) 
View(cw_att8scr_drugRate_2022_2023)

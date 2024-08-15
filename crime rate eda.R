library(tidyverse)
library(plotly)
library(lubridate)
#fmsb - for creating radar charts
install.packages("fmsb")
library(fmsb)

#Cleaned data sets
bristolCrimeSummary = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/bristol-crime-summary.csv")
View(bristolCrimeSummary)
cornwallCrimeSummary = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/cornwall-crime-summary.csv")
View(cornwallCrimeSummary)
bristolCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/bristol-crime-rate.csv")
View(bristolCrimeRate)
cornwallCrimeRate = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/crime rate/cornwall-crime-rate.csv")
View(cornwallCrimeRate)

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

#By checking the structure of bristolCrimeRate, it is observed that Year column is of chr data type
#Same goes for cornwallCrimeRate
str(bristolCrimeRate)

#For merging with population_2022, Postcode column is created 
  #by extracting first 5 characters (including the blank space)
#column Year is converted to Date data type and YYYY-MM-DD format
bristolCrimeRate = bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(bristolCrimeRate)
str(bristolCrimeRate)

cornwallCrimeRate = cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
View(cornwallCrimeRate)

#bcCrimeRate data set created by mering bristolCrimeRate and cornwallCrimeRate
bcCrimeRate = bind_rows(bristolCrimeRate, cornwallCrimeRate)
View(bcCrimeRate)


#-----------------------------------------------------------------------
#BOX PLOT OF DRUG OFFENCE RATE OF 2022 IN COUNTIES BRISTOL AND CORNWALL
#Data set drugOffenceRate created by filtering out year of 2022,
  #drug crime and Bristol and Cornwall cities
  #and merged with population_2022 data set
drugOffenceRate = bcCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of" | city == "Cornwall") 
View(drugOffenceRate)
colSums(is.na(drugOffenceRate))

#drugOffenceRate is refined by removing rows with NA population
drugOffenceRate = drugOffenceRate %>% 
  filter(!is.na(Population))

#Creation of box plot
drugOffenceRateBoxPlot = drugOffenceRate %>% 
  #Grouped by city and postcode to show detailed drug crime rates
  group_by(Postcode, city) %>%
  #Rows of drug offences in each Postcode of each city is counted
  #There are repetitions of population due to repetition of postcode
    #So the first occurence of population is counted to calculate total sum
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) %>%
  ggplot(aes(x = city, y = drug_offense_rate)) +
  geom_boxplot() +
  labs(title = "Distribution of Drug Offence Rates of 2022 in Bristol and Cornwall county",
       x = "County",
       y = "Drug Offence Rate per 10,000 people") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  scale_x_discrete(labels = c("Bristol, City of" = "Bristol",
                              "Cornwall" = "Cornwall")) +
  theme(plot.title = element_text(size = 12))
ggplotly(drugOffenceRateBoxPlot)


#------------------------------------------------------------------------------------
#RADAR CHART OF VEHICLE CRIME RATE PER 10,000 PEOPLE OF 2022 IN BRISTOL 
#vehicleCrimeRate data set created by filtering out Vehicle crime
bsVehicleCrimeRate = bristolCrimeRate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Vehicle crime") %>% 
  filter(city == "Bristol, City of") %>% 
  filter(!is.na(Population))
View(bsVehicleCrimeRate)
dim(bsVehicleCrimeRate)
colSums(is.na(bsVehicleCrimeRate))

#Data frame vehicleCrimeRateSummary created containing vehicle crime rate per 10,000 for each LSOA 
bsVehicleCrimeRateSummary = bsVehicleCrimeRate %>%
  group_by(`LSOA name`) %>%
  summarise(total_vehicleCrimes = n(), 
    population = first(Population),
    vehicleCrimeRate_per_10000 = (total_vehicleCrimes / population) * 10000) %>%
  ungroup()
View(bsVehicleCrimeRateSummary)

# Sample 20 LSOAs randomly
set.seed(123) # Set seed for reproducibility
bsSampled_lsoas = bsVehicleCrimeRateSummary %>%
  sample_n(20)

#Created dataframe for radar chart
bsRadar_data = bsSampled_lsoas %>%
  pivot_wider(names_from = `LSOA name`, values_from = vehicleCrimeRate_per_10000, values_fill = list(vehicleCrimeRate_per_10000 = 0)) %>%
  as.data.frame()

#Max values added for scaling and dummy variables
bsMax_values = rep(max(bsRadar_data[-1], na.rm = TRUE), ncol(bsRadar_data))
bsRadar_data = rbind(rep(0, ncol(bsRadar_data)), bsMax_values, bsRadar_data)
dim(bsRadar_data)

#Set up the radar chart
#Min and Max columns: to define the scale of the chart
  #i.e., maximum and minimum value for each axis
colnames(bsRadar_data) = c("Min", "Max", paste("Area", 1:(ncol(bsRadar_data) - 2), sep = " "))

#Plot radar chart
radarchart(bsRadar_data, 
           axistype = 1, 
           pcol = c("blue", "red"), 
           pfcol = c("blue", "red"), 
           plwd = 2, 
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           vlcex = 0.8, 
           title = "Vehicle Crime Rate per 10,000 People in Bristol (Nov 2022)",
           cex.main = 1)


#--------------------------------------------------------------------------------
#PIE CHART FOR ROBBERY CRIME RATES PER 10,000 IN 2022 FOR BRISTOL AND CORNWALL
#Data set robberyCrimeRate created by filtering out Burglary crime type
robberyCrimeRate = bcCrimeRate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Burglary") %>% 
  filter(city == "Bristol, City of" | city == "Cornwall") %>% 
  filter(!is.na(Population))
View(robberyCrimeRate)
dim(robberyCrimeRate)
colSums(is.na(robberyCrimeRate))

#Creation of pie chart
robberyCrimeRate %>%
  group_by(city) %>%
  summarise(total_burglaryCrimes = n(), 
            population = first(Population),
            burglaryCrimeRate_per_10000 = (total_burglaryCrimes / population) * 10000) %>% 
  ungroup() %>%
  #for adding percentages to the pie chart
  mutate(percentage = total_burglaryCrimes / sum(total_burglaryCrimes) * 100,
         label = paste0(round(percentage, 1), "%")) %>% 
  ggplot(aes(x = "", y = burglaryCrimeRate_per_10000, fill = city)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Burglary Crime Rate per 10,000 People in Bristol and Cornwall (Nov 2022)",
       fill = "County") +
  scale_fill_discrete(labels = c("Bristol, City of" = "Bristol",
                                 "Cornwall" = "Cornwall")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))

#Pie chart of robbery crime rates in LSOAs of Bristol in 2022
bsRobberyCrimeRate = bristolCrimeRate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Burglary") %>% 
  filter(city == "Bristol, City of") %>% 
  filter(!is.na(Population))
View(bsRobberyCrimeRate)
dim(bsRobberyCrimeRate)
colSums(is.na(bsRobberyCrimeRate))

bsRobberyCrimeRate %>%
  group_by(`LSOA name`) %>%
  summarise(total_burglaryCrimes = n(), 
            population = first(Population),
            burglaryCrimeRate_per_10000 = (total_burglaryCrimes / population) * 10000) %>% 
  ungroup() %>%
  mutate(percentage = total_burglaryCrimes / sum(total_burglaryCrimes) * 100,
         label = paste0(round(percentage, 1), "%")) %>% 
  slice_sample(n = 15) %>% 
  ggplot(aes(x = "", y = burglaryCrimeRate_per_10000, fill = `LSOA name`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Burglary Crime Rate per 10,000 People in Bristol (Nov 2022)",
       fill = "LSOA name") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))

#Pie chart of robbery crime rates in LSOAs of Cornwall in 2022
cwRobberyCrimeRate = cornwallCrimeRate %>% 
  filter(year(Year) == 2022 & month(Year) == 11) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Burglary") %>% 
  filter(city == "Cornwall") %>% 
  filter(!is.na(Population))
View(cwRobberyCrimeRate)
dim(cwRobberyCrimeRate)
colSums(is.na(cwRobberyCrimeRate))

cwRobberyCrimeRate %>%
  group_by(`LSOA name`) %>%
  summarise(total_burglaryCrimes = n(), 
            population = first(Population),
            burglaryCrimeRate_per_10000 = (total_burglaryCrimes / population) * 10000) %>% 
  ungroup() %>%
  mutate(percentage = total_burglaryCrimes / sum(total_burglaryCrimes) * 100,
         label = paste0(round(percentage, 1), "%")) %>% 
  slice_sample(n = 15) %>% 
  ggplot(aes(x = "", y = burglaryCrimeRate_per_10000, fill = `LSOA name`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Burglary Crime Rate per 10,000 People in Cornwall (Nov 2022)",
       fill = "LSOA name") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))


#---------------------------------------------------------------------------------
#LINE CHART OF DRUG OFFENCE RATES PER 10,000 PEOPLE IN BRISTOL AND CORNWALL(2022)
drugOffenceRate %>% 
  group_by(Year, city) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) %>%
  ggplot(aes(x = Year, y = drug_offense_rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Line Plot: Drug Offence Rates per 10,000 people in Bristol and Cornwall (2022)",
       x = "County",
       y = "Drug Offence Rate per 10,000 people") +
  facet_wrap(~city) +
  theme(plot.title = element_text(size = 12))

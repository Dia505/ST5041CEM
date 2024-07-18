library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(lubridate)

#FOR HOUSE PRICING DATA SET OF 2020
#The csv file is read from the given path 
#without considering the first row as titles
housePricing20 = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/house pricing/pp-2020.csv", col_names = FALSE)

view(housePricing20)
ncol(housePricing20)
nrow(housePricing20)

#Each column is provided a title
names(housePricing20) = c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                          "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                          "District", "County", "PPD_type", "Record_status")

view(housePricing20)
#For qualitative variables, to check data type, length and mode
#For quantitative variables, to check statistics 
summary(housePricing20)
#To check number of missing values for each column
colSums(is.na(housePricing20))

#To check the class attribute, which determines how the object will be handled by R's generic functions 
class(housePricing20$Date_of_transfer)
class(housePricing20$Price)

#The data type of Date_of_transfer is changed to Date
housePricing20$Date_of_transfer = ymd(housePricing20$Date_of_transfer)
class(housePricing20$Date_of_transfer)

#To handle missing values
housePricing20$Postcode[is.na(housePricing20$Postcode)] = "Unknown"
housePricing20$SAON[is.na(housePricing20$SAON)] = "Not Applicable"
housePricing20$Street[is.na(housePricing20$Street)] = "Unknown"
housePricing20$Locality[is.na(housePricing20$Locality)] = "Not Applicable"

head(housePricing20)
colSums(is.na(housePricing20))

#To check for any duplicate rows
sum(duplicated(housePricing20))

#house pricing data in City of Bristol in the year 2020
bsHousePrice20 = housePricing20 %>% 
  filter(County == "CITY OF BRISTOL")
view(bsHousePrice20)

#house pricing data in Cornwall in the year 2020
cwHousePrice20 = housePricing20 %>% 
  filter(County == "CORNWALL")
view(cwHousePrice20)

bcHousePrice20 = housePricing20 %>% 
  filter(County == "CITY OF BRISTOL" | County == "CORNWALL")
view(bcHousePrice20)


#---------------------------------
#FOR HOUSE PRICING DATA SET OF 2021
housePricing21 = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/house pricing/pp-2021.csv", col_names = FALSE)

head(housePricing21)
dim(housePricing21)

names(housePricing21) = c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                          "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                          "District", "County", "PPD_type", "Record_status")
head(housePricing21)

summary(housePricing21)
colSums(is.na(housePricing21))

class(housePricing21$Date_of_transfer)
class(housePricing21$Price)

housePricing21$Date_of_transfer = ymd(housePricing21$Date_of_transfer)
class(housePricing21$Date_of_transfer)

housePricing21$Postcode[is.na(housePricing21$Postcode)] = "Unknown"
housePricing21$SAON[is.na(housePricing21$SAON)] = "Not Applicable"
housePricing21$Street[is.na(housePricing21$Street)] = "Unknown"
housePricing21$Locality[is.na(housePricing21$Locality)] = "Not Applicable"

head(housePricing21)
colSums(is.na(housePricing21))

sum(duplicated(housePricing21))

bsHousePrice21 = housePricing21 %>% 
  filter(County == "CITY OF BRISTOL")
view(bsHousePrice21)

cwHousePrice21 = housePricing21 %>% 
  filter(County == "CORNWALL")
view(cwHousePrice21)

bcHousePrice21 = housePricing21 %>% 
  filter(County == "CITY OF BRISTOL" | County == "CORNWALL")
view(bcHousePrice21)


#---------------------------------
#FOR HOUSE PRICING DATA SET OF 2022
housePricing22 = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/house pricing/pp-2022.csv", col_names = FALSE)

head(housePricing22)
dim(housePricing22)

names(housePricing22) = c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                          "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                          "District", "County", "PPD_type", "Record_status")
head(housePricing22)

summary(housePricing22)
colSums(is.na(housePricing22))

class(housePricing22$Date_of_transfer)
class(housePricing22$Price)

housePricing22$Date_of_transfer = ymd(housePricing22$Date_of_transfer)
class(housePricing22$Date_of_transfer)

housePricing22$Postcode[is.na(housePricing22$Postcode)] = "Unknown"
housePricing22$SAON[is.na(housePricing22$SAON)] = "Not Applicable"
housePricing22$Street[is.na(housePricing22$Street)] = "Unknown"
housePricing22$Locality[is.na(housePricing22$Locality)] = "Not Applicable"

colSums(is.na(housePricing22))

sum(duplicated(housePricing22))

bsHousePrice22 = housePricing22 %>% 
  filter(County == "CITY OF BRISTOL")
view(bsHousePrice22)

cwHousePrice22 = housePricing22 %>% 
  filter(County == "CORNWALL")
view(cwHousePrice22)

bcHousePrice22 = housePricing22 %>% 
  filter(County == "CITY OF BRISTOL" | County == "CORNWALL")
view(bcHousePrice22)


#---------------------------------
#FOR HOUSE PRICING DATA SET OF 2023
housePricing23 = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/house pricing/pp-2023.csv", col_names = FALSE)

head(housePricing23)
dim(housePricing23)

names(housePricing23) = c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                          "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                          "District", "County", "PPD_type", "Record_status")
head(housePricing23)

summary(housePricing23)
colSums(is.na(housePricing23))

class(housePricing23$Date_of_transfer)
class(housePricing23$Price)

housePricing23$Date_of_transfer = ymd(housePricing23$Date_of_transfer)
class(housePricing23$Date_of_transfer)

housePricing23$Postcode[is.na(housePricing23$Postcode)] = "Unknown"
housePricing23$SAON[is.na(housePricing23$SAON)] = "Not Applicable"
housePricing23$Street[is.na(housePricing23$Street)] = "Unknown"
housePricing23$Locality[is.na(housePricing23$Locality)] = "Not Applicable"

head(housePricing23)
colSums(is.na(housePricing23))

sum(duplicated(housePricing23))

bsHousePrice23 = housePricing23 %>% 
  filter(County == "CITY OF BRISTOL")
view(bsHousePrice23)

cwHousePrice23 = housePricing23 %>% 
  filter(County == "CORNWALL")
view(cwHousePrice23)

bcHousePrice23 = housePricing23 %>% 
  filter(County == "CITY OF BRISTOL" | County == "CORNWALL")
view(bcHousePrice23)


#-----------------
#CLEANED DATA SETS
#using bind_rows(), the data sets are merged
bristolHousePriceCleaned = bind_rows(bsHousePrice20, bsHousePrice21, bsHousePrice22, bsHousePrice23)
view(bristolHousePriceCleaned)
dim(bristolHousePriceCleaned)

cornwallHousePriceCleaned = bind_rows(cwHousePrice20, cwHousePrice21, cwHousePrice22, cwHousePrice23)
view(cornwallHousePriceCleaned)
dim(cornwallHousePriceCleaned)

bcHousePriceCleaned = bind_rows(bcHousePrice20, bcHousePrice21, bcHousePrice22, bcHousePrice23)
view(bcHousePriceCleaned)
dim(bcHousePriceCleaned) 

#The final, cleaned data sets are saved to the given path using write.csv()
write.csv(bristolHousePriceCleaned, "C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-house-pricing.csv", row.names = FALSE)
write.csv(cornwallHousePriceCleaned, "C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/cornwall-house-pricing.csv", row.names = FALSE)
write.csv(bcHousePriceCleaned, "C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/house pricing/bristol-cornwall-house-pricing.csv", row.names = FALSE)

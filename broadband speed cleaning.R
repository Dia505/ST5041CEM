library(tidyverse)
library(readr)
library(dplyr)

#FOR BROADBAND SPEED PERFORMANCE
broadbandSpeedPerformance = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/broadband speed/201805_fixed_pc_performance_r03.csv")
View(broadbandSpeedPerformance)
dim(broadbandSpeedPerformance)

colnames(broadbandSpeedPerformance)

summary(broadbandSpeedPerformance)
colSums(is.na(broadbandSpeedPerformance))

str(broadbandSpeedPerformance)

#On looking at the number of missing values in each column, it is not constant and varies from column to column
#In this dataset, the columns with numeric data type have missing values

#To handle columns with missing values that are less than 90% in number, 
  #the missing values are filled with the median of the column
  #Median maintains the central tendency of the data
  #Median is also less sensitive to outliers, and doesn't allow their disproportionate influence

#On countering columns which constitute missing values that are more than 90%
  #the missing values are filled with 0
#This is because the small amount of available data may not represent the overall distribution.
  #Filling the missing values with the median from the little available data can distort the distribution of data
broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), 
                         ifelse(sum(is.na(.)) > 0.9 * length(.), 0, median(., na.rm = TRUE)), 
                         .)))

View(broadbandSpeedPerformance)
colSums(is.na(broadbandSpeedPerformance))

#The data set has no duplicated rows
sum(duplicated(broadbandSpeedPerformance))

#The summary statistics of the data set is checked again to see any changes due to the removal of missing values
#The summary also shows a huge difference between 3rd quartile and max outlier
#Such outliers can influence the data set and cause skewness in visualization
summary(broadbandSpeedPerformance)

#Histogram is created for Average data usage (GB) 
  #which showed right skewness
ggplot(broadbandSpeedPerformance, aes(x = `Average data usage (GB)`)) +
  geom_histogram() +
  ggtitle("Original Average Data Usage Distribution")

#Log transformation has been done to make highly skewed distribution less skewed
  #and to normalize or adjust the scale of the column's values
#The natural logarithm of the values are calculated
#1 is added to avoid issues when the data contains zeros (since log(0) is undefined)
broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(
    `Average data usage (GB)` = log(`Average data usage (GB)` + 1)
  )

#The log transformation brought more normality/symmetry in the data distribution
ggplot(broadbandSpeedPerformance, aes(x = `Average data usage (GB)`)) +
  geom_histogram() +
  ggtitle("Original Average Data Usage Distribution")

#Checking the summary again, the difference between 3rd quartile and max outlier also decreased
summary(broadbandSpeedPerformance)

#Log transformation done similarly on other columns with the same issue
broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(
    `Median download speed (Mbit/s)` = log(`Median download speed (Mbit/s)` + 1),
    `Average download speed (Mbit/s)` = log(`Average download speed (Mbit/s)` + 1),
    `Minimum download speed (Mbit/s)` = log(`Minimum download speed (Mbit/s)` + 1),
    `Maximum download speed (Mbit/s)` = log(`Maximum download speed (Mbit/s)` + 1)
  )
summary(broadbandSpeedPerformance)

broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(
    `Median upload speed (Mbit/s)` = log(`Median upload speed (Mbit/s)` + 1),
    `Average upload speed (Mbit/s)` = log(`Average upload speed (Mbit/s)` + 1),
    `Minimum upload speed (Mbit/s)` = log(`Minimum upload speed (Mbit/s)` + 1),
    `Maximum upload speed (Mbit/s)` = log(`Maximum upload speed (Mbit/s)` + 1),
    `Average upload speed (Mbit/s) for lines 10<30Mbit/s` = log(`Average upload speed (Mbit/s) for lines 10<30Mbit/s` + 1),
    `Average upload speed (Mbit/s) for SFBB lines` = log(`Average upload speed (Mbit/s) for SFBB lines` + 1)
  )
summary(broadbandSpeedPerformance)

broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(
    `Number of connections 5<10 Mbit/s (number of lines)` = log(`Number of connections 5<10 Mbit/s (number of lines)` + 1),
    `Number of connections 10<30 Mbit/s (number of lines)` = log(`Number of connections 10<30 Mbit/s (number of lines)` + 1),
    `Number of connections >= 30 Mbit/s (number of lines)` = log(`Number of connections >= 30 Mbit/s (number of lines)` + 1)
  )
summary(broadbandSpeedPerformance)

broadbandSpeedPerformance = broadbandSpeedPerformance %>%
  mutate(
    `Average data usage (GB) for lines < 10Mbit/s` = log(`Average data usage (GB) for lines < 10Mbit/s` + 1),
    `Average data usage (GB) for Basic BB lines` = log(`Average data usage (GB) for Basic BB lines` + 1),
    `Average data usage (GB) for SFBB lines` = log(`Average data usage (GB) for SFBB lines` + 1)
  )
summary(broadbandSpeedPerformance)

#Broadband speed performance data in Bristol
bsBroadbandSpeedPerf = broadbandSpeedPerformance %>% 
  filter(`postcode area` == "BS")
View(bsBroadbandSpeedPerf)

#Broadband speed performance data in Cornwall
cwBroadbandSpeedPerf = broadbandSpeedPerformance %>% 
  filter(`postcode area` == "TR" | `postcode area` == "PL")
View(cwBroadbandSpeedPerf)

#Broadband speed performance data in Bristol and Cornwall
bcBroadbandSpeedPerf = broadbandSpeedPerformance %>% 
  filter(`postcode area` == "BS" | `postcode area` == "TR" | `postcode area` == "PL")
View(bcBroadbandSpeedPerf)


#----------------------------
#FOR BROADBAND SPEED COVERAGE
broadbandSpeedCoverage = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/broadband speed/201809_fixed_pc_coverage_r01.csv")
View(broadbandSpeedCoverage)
dim(broadbandSpeedCoverage)
colnames(broadbandSpeedCoverage)

#Summary of the data set tells us that for columns with high difference between 3rd quartile and max outlier
  #most of the statistical values are 0
#This rather entails that  extreme values like 100% are not outliers 
  #but rather part of the distribution due to the nature of the data 
    #(e.g., geographical areas with poor broadband coverage)
summary(broadbandSpeedCoverage)

#colSums(is.na(broadbandSpeedCoverage)) showed no missing values in any column of the data set
colSums(is.na(broadbandSpeedCoverage))
str(broadbandSpeedCoverage)

#The column pcds is renamed to postcode_space and column pca is renamed to postcode area
  #to match the column names of data set broadbandSpeedPerformance for merging
broadbandSpeedCoverage = broadbandSpeedCoverage %>% 
  rename(`postcode_space` = `pcds`, 
         `postcode area` = `pca`)
#The column All Matched Premises is renamed to Connected Premises for better understanding
broadbandSpeedCoverage = broadbandSpeedCoverage %>% 
  rename(`Connected Premises` = `All Matched Premises`)
colnames(broadbandSpeedCoverage)

#The data set has no duplicated rows
sum(duplicated(broadbandSpeedPerformance))

#Broadband speed coverage data in Bristol
bsBroadbandSpeedCov = broadbandSpeedCoverage %>% 
  filter(`postcode area` == "BS")
View(bsBroadbandSpeedCov)

#Broadband speed coverage data in Cornwall
cwBroadbandSpeedCov = broadbandSpeedCoverage %>% 
  filter(`postcode area` == "TR" | `postcode area` == "PL")
View(cwBroadbandSpeedCov)

#Broadband speed coverage data in Bristol and Cornwall
bcBroadbandSpeedCov = broadbandSpeedCoverage %>% 
  filter(`postcode area` == "BS" | `postcode area` == "TR" | `postcode area` == "PL")
View(bcBroadbandSpeedCov)


#-------------------
#CLEANED DATA SETS
#The data sets have been merged using inner join via the common columns postcode, postcode_space and postcode are
#Inner join only merges rows with matching values in both data sets based on the key columns specified
#Using full join resulted in missing values in the cleaned data sets
  #whereas by using inner join, there were no missing values

#For Bristol
bristolBroadbandSpeedCleaned = inner_join(bsBroadbandSpeedPerf, bsBroadbandSpeedCov,
                         by = c("postcode", "postcode_space", "postcode area"))
View(bristolBroadbandSpeedCleaned)
colSums(is.na(bristolBroadbandSpeedCleaned))
dim(bristolBroadbandSpeedCleaned)

#The data set is cleaned further to remove unnecessary columns 
bristolBroadbandSpeedCleaned = bristolBroadbandSpeedCleaned %>% 
  select(postcode, postcode_space, `postcode area`, 
         `Average download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Maximum upload speed (Mbit/s)`, `Minimum upload speed (Mbit/s)`,
         `Average data usage (GB)`, `All Premises`, `Connected Premises`)
View(bristolBroadbandSpeedCleaned)
dim(bristolBroadbandSpeedCleaned)

#For Cornwall
cornwallBroadbandSpeedCleaned = inner_join(cwBroadbandSpeedPerf, cwBroadbandSpeedCov,
                                          by = c("postcode", "postcode_space", "postcode area"))
View(cornwallBroadbandSpeedCleaned)
colSums(is.na(cornwallBroadbandSpeedCleaned))
dim(cornwallBroadbandSpeedCleaned)

cornwallBroadbandSpeedCleaned = cornwallBroadbandSpeedCleaned %>% 
  select(postcode, postcode_space, `postcode area`, 
         `Average download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Maximum upload speed (Mbit/s)`, `Minimum upload speed (Mbit/s)`,
         `Average data usage (GB)`, `All Premises`, `Connected Premises`)
View(cornwallBroadbandSpeedCleaned)
dim(cornwallBroadbandSpeedCleaned)

#For Bristol and Cornwall
bcBroadbandSpeedCleaned = inner_join(bcBroadbandSpeedPerf, bcBroadbandSpeedCov,
                                           by = c("postcode", "postcode_space", "postcode area"))
View(bcBroadbandSpeedCleaned)
colSums(is.na(bcBroadbandSpeedCleaned))
dim(bcBroadbandSpeedCleaned)

bcBroadbandSpeedCleaned = bcBroadbandSpeedCleaned %>% 
  select(postcode, postcode_space, `postcode area`, 
         `Average download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Maximum upload speed (Mbit/s)`, `Minimum upload speed (Mbit/s)`,
         `Average data usage (GB)`, `All Premises`, `Connected Premises`)
View(bcBroadbandSpeedCleaned)
dim(bcBroadbandSpeedCleaned)

#For postcode to LSOA code
pscdToLsoa = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Datasets/population/Postcode to LSOA.csv")
View(pscdToLsoa)
dim(pscdToLsoa)
colnames(pscdToLsoa)

#To check if lsoa11nm has values of towns or certain neighbourhoods
print(unique(pscdToLsoa$lsoa11nm))

#pcds renamed to postcode_space for left_join
#lsoa11nm renamed to lsoa_area, it's values don't correspond to towns
#ladnm renamed to cities
#The data frame is narrowed down to relevant columns
pscdToLsoa = pscdToLsoa %>% 
  rename(postcode_space = pcds) %>% 
  rename(lsoa_area = lsoa11nm) %>% 
  rename(city = ladnm) %>% 
  select(postcode_space, lsoa_area, city)
View(pscdToLsoa)
colSums(is.na(pscdToLsoa))

#Using left_join, names of lsoa areas and cities are added to all cleaned data sets of Bristol, Cornwall and both
bristolBroadbandSpeedCleaned = bristolBroadbandSpeedCleaned %>% 
  left_join(pscdToLsoa, by = "postcode_space")
View(bristolBroadbandSpeedCleaned)
colnames(bristolBroadbandSpeedCleaned)
dim(bristolBroadbandSpeedCleaned)

cornwallBroadbandSpeedCleaned = cornwallBroadbandSpeedCleaned %>% 
  left_join(pscdToLsoa, by = "postcode_space")
View(cornwallBroadbandSpeedCleaned)

bcBroadbandSpeedCleaned = bcBroadbandSpeedCleaned %>% 
  left_join(pscdToLsoa, by = "postcode_space")
View(bcBroadbandSpeedCleaned)

#Cleaned data sets saved in csv format
write.csv(bristolBroadbandSpeedCleaned, "C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-broadband-speed.csv", row.names = FALSE)
write.csv(cornwallBroadbandSpeedCleaned, "C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/cornwall-broadband-speed.csv", row.names = FALSE)
write.csv(bcBroadbandSpeedCleaned, "C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-cornwall-broadband-speed.csv", row.names = FALSE)

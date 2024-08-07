library(tidyverse)
library(plotly)

#Loading clean data sets
bristolSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/bristol-schools.csv")
View(bristolSchools)
cornwallSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/cornwall-schools.csv")
View(cornwallSchools)

#BOX PLOT: AVERAGE ATTAINMENT 8 SCORES OF 2022 ACROSS SCHOOLS BRISTOL AND CORNWALL
options(scipen = 1000)

#By checking summary of the data sets, it was discovered that
  #ATT8SCR is a character
summary(bristolSchools)
summary(cornwallSchools)

#The two data sets are merged for creating two box plots in one image
att8scr2022BoxPlot = bind_rows(bristolSchools, cornwallSchools) %>% 
  #SUPP and NE values of ATT8SCR are removed because they aren't valid, established values
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  #The values of ATT8SCR are converted to numeric form
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  #Grouped by SCHNAME to calculate average ATT8SCR of each school
  group_by(SCHNAME, COUNTY) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>% 
  ggplot(aes(x = COUNTY, y = avg_ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Attainment 8 Scores in the Academic Session 2021-2022",
       x = "County",
       y = "Attainment 8 Scores") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))
  
ggplotly(att8scr2022BoxPlot, tooltip = "text")  

#LINE PLOT: AVERAGE ATTAINMENT 8 SCORES OF SCHOOLS IN BRISTOL AND CORNWALL (2022)
#FOR BRISTOL COUNTY
bristolSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>%
  #group = 1: draws a single line to connect all the data points
    #groups the data points into a single group
  ggplot(aes(x = SCHNAME, y = avg_ATT8SCR, group = 1)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores across Schools of Bristol in 2021-2022",
       x = "Schools",
       y = "Average Attainment 8 Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

#FOR CORNWALL COUNTY
cornwallSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>%
  ggplot(aes(x = SCHNAME, y = avg_ATT8SCR, group = 1)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores across Schools of Cornwall in 2021-2022",
       x = "Schools",
       y = "Average Attainment 8 Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))


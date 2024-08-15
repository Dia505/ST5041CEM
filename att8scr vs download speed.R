library(tidyverse)

bristolBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/bristol-broadband-speed.csv")
View(bristolBroadbandSpeed)
cornwallBroadbandSpeed = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/broadband speed/cornwall-broadband-speed.csv")
View(cornwallBroadbandSpeed)

bristolSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/bristol-schools.csv")
View(bristolSchools)
cornwallSchools = read_csv("C:/Users/dell/OneDrive/Desktop/ST5014CEM/Clean data/schools/cornwall-schools.csv")
View(cornwallSchools)

bristolSchools2022 = bristolSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))
View(bristolSchools2022)
colSums(is.na(bristolSchools2022))

cornwallSchools2022 = cornwallSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))
View(cornwallSchools2022)
colSums(is.na(cornwallSchools2022))

#LINEAR MODELLING
#FOR SCHOOLS IN BRISTOL COUNTY
bs_att8scr_downloadSpeed_2022 = inner_join(
  bristolSchools2022, bristolBroadbandSpeed,
  by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, `Average download speed (Mbit/s)`)
View(bs_att8scr_downloadSpeed_2022)

#Weak negative relationship (-0.284)
bs_att8scr_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average download speed (Mbit/s)`))

bsModel_att8scr_downloadSpeed = lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = bs_att8scr_downloadSpeed_2022)
summary(bsModel_att8scr_downloadSpeed)

bsIntercept_att8scr_downloadSpeed = coef(bsModel_att8scr_downloadSpeed)[1]
bsSlope_att8scr_downloadSpeed = coef(bsModel_att8scr_downloadSpeed)[2]

ggplot(bs_att8scr_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = bsIntercept_att8scr_downloadSpeed, slope = bsSlope_att8scr_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on Attainment 8 Score in Bristol Schools (2022)",
    x = "Average Download Speed",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

#FOR SCHOOLS IN CORNWALL COUNTY
cw_att8scr_downloadSpeed_2022 = inner_join(
  cornwallSchools2022, cornwallBroadbandSpeed,
  by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, `Average download speed (Mbit/s)`)
View(cw_att8scr_downloadSpeed_2022)

#Strong positive relationship (0.858)
cw_att8scr_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average download speed (Mbit/s)`))

cwModel_att8scr_downloadSpeed = lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = cw_att8scr_downloadSpeed_2022)
summary(cwModel_att8scr_downloadSpeed)

cwIntercept_att8scr_downloadSpeed = coef(cwModel_att8scr_downloadSpeed)[1]
cwSlope_att8scr_downloadSpeed = coef(cwModel_att8scr_downloadSpeed)[2]

ggplot(cw_att8scr_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = cwIntercept_att8scr_downloadSpeed, slope = cwSlope_att8scr_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on Attainment 8 Score in Cornwall Schools (2022)",
    x = "Average Download Speed",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

library(VAST)
library(tidyverse)

load("Data/CPUE_5.RData")

CPUE <- Data_Geostat %>%
  group_by(Year) %>%
  summarise(cpue=mean(Catch_KG))

write.csv(CPUE,file="Data/Nominal_CPUE.csv",row.names = FALSE)
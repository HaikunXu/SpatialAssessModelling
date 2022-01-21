# Compare CPUE

library(tidyverse)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_1area_observations_1_100_v2.RData")

data <- dat_1A_1

CPUE0 <- data.frame(data$CPUE)

CPUE <- read.csv("Data/VAST_Index/Table_for_SS3.csv")

ggplot() +
  geom_line(aes(x=as.numeric(Year),y=Estimate_metric_tons/mean(Estimate_metric_tons)),data=CPUE,color="blue") +
  geom_line(aes(x=as.numeric(levels(year))[year],y=cpu),data=CPUE0,color="red") +
  theme_bw()

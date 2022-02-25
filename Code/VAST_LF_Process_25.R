library(tidyverse)

data <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_25/Table_for_SS3.csv")

LF <- data %>% mutate(Length=Category*10+10) %>%
  group_by(Year) %>% mutate(Index=sum(Estimate_metric_tons)) %>%
  filter(Index>0) %>%
  mutate(LF=Estimate_metric_tons/Index) %>%
  select(Year,Length,LF) %>%
  spread(Length,LF)

Nsamp <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_25/Nsamp.csv")

LF_SS <- data.frame(method=1,
                    Yr=LF$Year,
                    Seas=1,
                    Flt=17,
                    Gender=0,
                    Part=0,
                    Nsamp=Nsamp$N*5)

LF_SS <- cbind(LF_SS,LF[,-1])

write.csv(LF_SS,file="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_25/SS.csv",row.names = FALSE)


library(tidyverse)

data <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/VAST_LF/Table_for_SS3.csv")

LF <- data %>% mutate(Length=Category*10+20) %>%
  group_by(Year) %>% mutate(Index=sum(Estimate_metric_tons)) %>%
  filter(Index>0) %>%
  mutate(LF=Estimate_metric_tons/Index) %>%
  select(Year,Length,LF) %>%
  spread(Length,LF)

LF_SS <- data.frame(method=1,
                    Yr=LF$Year+80,
                    Seas=1,
                    Flt=17,
                    Gender=0,
                    Part=0,
                    Nsamp=5)

LF_SS <- cbind(LF_SS,LF[,-1])

write.csv(LF_SS,file="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/VAST_LF/SS.csv",row.names = FALSE)

  
ggplot(data=LF) +
  geom_point(aes(x=Length,y=LF)) +
facet_wrap(~Year) +
  coord_cartesian(ylim = c(0,0.4))

LF_mean <- LF %>% group_by(Length) %>% summarise(LF_mean=mean(LF))

ggplot(data=LF_mean) +
  geom_point(aes(x=Length,y=LF_mean))


### Nominal
LL_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Spatial-SA-IATTC/Data/LL_LF_Nominal.csv")

library(tidyverse)

for (i in 11:50) {
data <- read.csv(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/Loop/",i,"/Table_for_SS3.csv"))

LF <- data %>% mutate(Length=Category*10+30) %>%
  group_by(Year) %>% mutate(Index=sum(Estimate_metric_tons)) %>%
  filter(Index>0) %>%
  mutate(LF=Estimate_metric_tons/Index) %>%
  select(Year,Length,LF) %>%
  spread(Length,LF)

Nsamp <- read.csv(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/Loop/",i,"/Nsamp.csv"))

LF_SS <- data.frame(method=1,
                    Yr=LF$Year,
                    Seas=1,
                    Flt=17,
                    Gender=0,
                    Part=0,
                    Nsamp=Nsamp$N*5)

LF_SS <- cbind(LF_SS,LF[,-1])

write.csv(LF_SS,file=paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/Loop/",i,"/SS.csv"),row.names = FALSE)

}

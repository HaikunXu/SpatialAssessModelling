# Regression Tree for LL

library(tidyverse)

for (i in 1:100) {
### LL
save_dir <- paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/")
load(paste0(save_dir,"LL_LF_5.RData"))

LF_raw <- LF_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  select(year,quarter,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon)

LF <- LF_DF[,3:7] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(40, 170, 10),220),right = F,labels = seq(30, 170, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(lf=sum(LF)) %>%
  group_by(year,quarter,Lat,Lon) %>% mutate(lf_sum=sum(lf)) %>%
  filter(lf_sum>0) %>%
  mutate(LF=lf/lf_sum) %>%
  select(year,quarter,Lat,Lon,Length2,LF) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon) %>% data.frame()


# load LL catch data
load(paste0(save_dir,"LL_Catch_5.RData"))
Catch <- Catch_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  rename(lat=Lat,lon=Lon) %>%
  select(year,quarter,lat,lon,Catch)


# sample size
LF_raw <- LF_DF %>%
  select(Year,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon) %>%
  group_by(Year,lat,lon) %>%
  mutate(tot=sum(LF)) %>%
  filter(tot>0)


LF_raw$Region <- 1

N <- LF_raw %>% mutate(Cell=Region) %>%
  group_by(Cell,Year) %>%
  summarise(n=length(unique(c(lat,lon))))

write.csv(N,file=paste0(save_dir,"LL_5_N_onearea.csv"),row.names = FALSE)

}

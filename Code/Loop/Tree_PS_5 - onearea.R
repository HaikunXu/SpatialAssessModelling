# Regression Tree for PS

library(tidyverse)

for (i in 1:100) {
  ### Purse-seine
  save_dir <- paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/")
  load(paste0(save_dir,"PS_LF_5.RData"))

LF_raw <- LF_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  select(year,quarter,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon) %>%
  group_by(year,quarter,lat,lon) %>%
  mutate(tot=sum(LF)) %>%
  filter(tot>0) # remove all-0 rows

# LF <- LF_DF[,c(3:7)] %>%
#   mutate(Length2=cut(Length,breaks = c(0,seq(40, 150, 10),220),right = F,labels = seq(30, 150, 10))) %>%
#   mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
#   group_by(year,quarter,Lat,Lon) %>%
#   mutate(tot=sum(LF)) %>%
#   filter(tot>0) %>% # remove all-0 rows  
#   group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
#   spread(Length2,LF) %>% rename(lat=Lat,lon=Lon) %>% data.frame()

# load PS catch data
load(paste0(save_dir,"PS_Catch_5.RData"))
Catch <- Catch_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  rename(lat=Lat,lon=Lon) %>%
  select(year,quarter,lat,lon,Catch)

Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

Catch$Cell <- 1

# catch weighted
LF_raw_cell <- left_join(LF_raw,Catch) %>%
  mutate(LF_raised = LF * Catch) %>% # catch weighted
  # mutate(LF_raised = LF) %>%
  select(year,quarter,lat,lon,Cell,Length,LF_raised)

# combine the flag with catch data and then group catch
LF_Fishery <- LF_raw_cell %>%
  group_by(Cell,year,quarter,Length) %>%
  summarise(lf_raised=sum(LF_raised),
            n=length(unique(c(lat,lon)))) %>%
  group_by(Cell,year,quarter) %>%
  mutate(lf_sum=sum(lf_raised)) %>%
  filter(lf_sum > 0) %>%
  mutate(lf=lf_raised/lf_sum) %>%
  select(Cell,year,quarter,n,Length,lf) %>%
  spread(Length,lf)

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF_5_onearea_cw.csv"),row.names = FALSE)

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

write.csv(N,file=paste0(save_dir,"PS_5_N_onearea.csv"),row.names = FALSE)

}
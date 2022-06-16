# Regression Tree for LL

library(tidyverse)

for (i in 11:100) {
### LL
save_dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/")
load(paste0(save_dir,"LL_LF_25.RData"))

reg <- function(Lat,Lon) {
  Region <- rep(2,length(Lat))
  
  Region[which(Lat>(-10)&Lon<60)] <- 1
  Region[which(Lat>(-15)&Lon<75&Lon>60)] <- 1
  
  Region[which(Lat>(-15)&Lon>75)] <- 4
  
  Region[which(Lat<(-30)&Lon>40)] <- 3
  Region[which(Lat>(-30)&Lat<(-15)&Lon>60)] <- 3
  
  return(Region)
}

# load LL catch data
load(paste0(save_dir,"LL_Catch_25.RData"))
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

reg <- function(Lat,Lon) {
  Region <- rep(2,length(Lat))
  
  Region[which(Lat>(-10)&Lon<60)] <- 1
  Region[which(Lat>(-15)&Lon<75&Lon>60)] <- 1
  
  Region[which(Lat>(-15)&Lon>75)] <- 4
  
  Region[which(Lat<(-30)&Lon>40)] <- 3
  Region[which(Lat>(-30)&Lat<(-15)&Lon>60)] <- 3
  
  return(Region)
}

LF_raw$Region <- reg(Lat=LF_raw$lat,Lon=LF_raw$lon)

N <- LF_raw %>% mutate(Cell=Region) %>%
  group_by(Cell,Year) %>%
  summarise(n=length(unique(c(lat,lon))))

write.csv(N,file=paste0(save_dir,"LL_25_N.csv"),row.names = FALSE)

}
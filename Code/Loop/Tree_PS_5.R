# Regression Tree for PS

library(tidyverse)

for (i in 11:100) {
### Purse-seine
save_dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/")
load(paste0(save_dir,"PS_LF_5.RData"))

LF_raw <- LF_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  select(year,quarter,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon) %>%
  group_by(year,quarter,lat,lon) %>%
  mutate(tot=sum(LF)) %>%
  filter(tot>0) # remove all-0 rows
  
# regression tree result
reg_rt <- function(Lat,Lon) {
  Region <- rep(2,length(Lat))

  Region[which(Lat<(-5))] <- 1
  Region[which(Lat>0)] <- 3

  return(Region)
}

# load PS catch data
load(paste0(save_dir,"PS_Catch_5.RData"))
Catch <- Catch_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  rename(lat=Lat,lon=Lon) %>%
  select(year,quarter,lat,lon,Catch)

Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

Catch$Cell <- reg_rt(Catch$lat,Catch$lon)

# combine the flag with catch data and then group catch
Catch_Fishery <- Catch %>%
  group_by(year,quarter,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

write.csv(Catch_Fishery,file=paste0(save_dir,"PS_Catch_5.csv"),row.names = FALSE)


# sample weighted
LF_raw_cell <- left_join(LF_raw,Catch) %>%
  # mutate(LF_raised = LF * Catch) %>% # catch weighted
  mutate(LF_raised = LF) %>%
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

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF_5.csv"),row.names = FALSE)


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

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF_5_cw.csv"),row.names = FALSE)

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

write.csv(N,file=paste0(save_dir,"PS_5_N.csv"),row.names = FALSE)

}
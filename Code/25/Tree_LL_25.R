# Regression Tree for LL

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)
library(FishFreqTree)

### LL
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_LF_25.RData")

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

fcol <- 5 # the first column with LF_Tree info
lcol <- 19 # the last column with LF_Tree info
bins <- seq(30,170,10)
save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/"

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
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_Catch_25.RData")
Catch <- Catch_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  rename(lat=Lat,lon=Lon) %>%
  select(year,quarter,lat,lon,Catch)

# use the regression tree package to group catch
LF$dummy = FALSE

Catch_Grid <- cbind(Catch[,1:4],matrix(0,nrow=nrow(Catch),ncol=lcol-fcol+1))
Catch_Grid$dummy = TRUE
names(Catch_Grid) <- names(LF)

# LF[6:8,fcol:lcol] <- LF[6:8,fcol:lcol] / 2 
# LF_new include dummy data for catch allocation
LF_new <- rbind(LF,Catch_Grid)
LF_new$lat <- as.numeric(LF_new$lat)
LF_new$lon <- as.numeric(LF_new$lon)

# LF_new$lat[which(LF_new$dummy==FALSE)] <- LF_new$lat[which(LF_new$dummy==FALSE)] + 5
# LF_new$lon[which(LF_new$dummy==FALSE)] <- LF_new$lon[which(LF_new$dummy==FALSE)] + 10

# select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])

# LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)
# make.split.map(LF_Tree$LF,Nsplit,save_dir)

# ggsave(file=paste0(save_dir,"Trees_5.png"), width = 10, height = 8)

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

write.csv(N,file=paste0(save_dir,"25_N.csv"),row.names = FALSE)

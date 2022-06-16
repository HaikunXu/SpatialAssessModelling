# Regression Tree for PS

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)

### Purse-seine
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_LF_5.RData")

LF_raw <- LF_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  select(year,quarter,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon) %>%
  group_by(year,quarter,lat,lon) %>%
  mutate(tot=sum(LF)) %>%
  filter(tot>0) # remove all-0 rows
  
LF <- LF_DF[,c(3:7)] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(40, 150, 10),220),right = F,labels = seq(30, 150, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon) %>%
  mutate(tot=sum(LF)) %>%
  filter(tot>0) %>% # remove all-0 rows  
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon) %>% data.frame()

# save(LF,file="LF.RData")

library(FishFreqTree)

fcol <- 5 # the first column with LF_Tree info
lcol <- 17 # the last column with LF_Tree info
bins <- seq(30, 150, 10)
Nsplit <- 3 # the number of splits (the number of cells - 1)
dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/")
save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/"

# run the regression tree
my_select_matrix <- data.matrix(expand.grid(
  select1 = 1:2,
  select2 = 1:1,
  select3 = 1:1))

area <- function(Lat,Lon) {
  area <- rep(1,length(Lat))
  
  i4 <- which(Lat > -15 & Lon > 75)
  area[i4] <- 4
  
  i2 <- which(Lat > -40 & Lat < (-10) & Lon < 40) 
  area[i2] <- 2
  i2 <- which(Lat > -30 & Lat < (-10) & Lon < 60) 
  area[i2] <- 2
  
  i3 <- which(Lat > -40 & Lat < (-30) & Lon > 40) 
  area[i3] <- 3
  i3 <- which(Lat > -30 & Lat < (-15) & Lon > 60) 
  area[i3] <- 3
  
  return(area)
}

# load PS catch data
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_Catch_5.RData")
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

LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir,include_dummy = TRUE, quarter = FALSE)

LF_Tree$LF$Flag3 <- area(LF_Tree$LF$lat,LF_Tree$LF$lon)

N <- LF_Tree$LF %>% filter(dummy==FALSE) %>%
  mutate(Cell=ifelse(Flag3<3,Flag3,3)) %>%
  group_by(Cell,year,quarter) %>%
  summarise(n=length(unique(c(lat,lon))))

# extract catch flag derived from the regression tree package
Cell <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]
Cell[which(Cell>3)] <- 3

# combine the flag with catch data and then group catch
Catch_Fishery <- cbind(Catch, Cell) %>%
  group_by(year,quarter,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

Catch_cell <- cbind(Catch, Cell)

# catch weighted
LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
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

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF_5_cw_nort.csv"),row.names = FALSE)

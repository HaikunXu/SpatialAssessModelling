# Regression Tree for PS

library(tidyverse)
library(FishFreqTree)

for (i in 33:100) {
  ### Purse-seine
  load(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/PS_LF_25.RData"))
  
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
  
  fcol <- 5 # the first column with LF_Tree info
  lcol <- 17 # the last column with LF_Tree info
  bins <- seq(30, 150, 10)
  Nsplit <- 3 # the number of splits (the number of cells - 1)
  dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/Loop/")
  save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/Loop/"
  
  # run the regression tree
  my_select_matrix <- data.matrix(expand.grid(
    select1 = 1:2,
    select2 = 1:2,
    select3 = 1:1))
  
  LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix, quarter = FALSE)
  
  # LF_Loop$LF_Tree$LF$Flag3 <- ifelse(LF_Loop$LF_Tree$LF$Flag3>3,3,LF_Loop$LF_Tree$LF$Flag3)
  
  make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir,plot_name = paste0(i,".png"))
}

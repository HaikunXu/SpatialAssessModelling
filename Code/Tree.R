# Regression Tree

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)

### Purse-seine
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_LF.RData")

LF <- LF_DF[,3:7] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(30, 150, 10),220),right = F,labels = seq(20, 150, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon) %>% data.frame()

# save(LF,file="LF.RData")

library(FishFreqTree)

fcol <- 5 # the first column with LF_Tree info
lcol <- 18 # the last column with LF_Tree info
bins <- seq(20,150,10)
Nsplit <- 3 # the number of splits (the number of cells - 1)
dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/")
save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/"

# load PS catch data
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_Catch.RData")
Catch <- Catch_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  rename(lat=Lat,lon=Lon) %>%
  select(year,quarter,lat,lon,Catch)

# use the regression tree package to group catch
LF$dummy = FALSE

Catch_Grid <- cbind(Catch[,1:4],matrix(0,nrow=nrow(Catch),ncol=lcol-fcol+1))
Catch_Grid$dummy = TRUE
names(Catch_Grid) <- names(LF)

LF[6:8,fcol:lcol] <- LF[6:8,fcol:lcol] / 2 
# LF_new include dummy data for catch allocation
LF_new <- rbind(LF,Catch_Grid)
LF_new$lat <- as.numeric(LF_new$lat)
LF_new$lon <- as.numeric(LF_new$lon)

# run the regression tree
# LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir, include_dummy = TRUE)
LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,max_select = 2)
f1 <- make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)

select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])

LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)

# extract catch flag derived from the regression tree package
Cell <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]

# combine the flag with catch data and then group catch
Catch_Fishery <- cbind(Catch, Cell) %>%
  group_by(year,quarter,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

Catch_Fishery_plot <- cbind(Catch, Cell) %>%
  group_by(year,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

f2 <- ggplot(data=Catch_Fishery_plot) +
  geom_line(aes(x=year,y=Total_Catch,color=Cell)) +
  theme_bw()

# catch-weighted tree
Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

LF_weight <- left_join(LF,Catch) %>%
  rename(weight=Catch) %>%
  mutate(weight=weight/mean(weight))

LF_Loop <- loop_regression_tree(LF_weight,fcol,lcol,bins,Nsplit,save_dir,max_select = 2)
f3 <- make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)

select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])

Catch_Grid$weight <- 1
LF_weight_new <- rbind(LF_weight,Catch_Grid)
LF_weight_new$lat <- as.numeric(LF_weight_new$lat)
LF_weight_new$lon <- as.numeric(LF_weight_new$lon)

LF_Tree <- run_regression_tree(LF_weight_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)

Cell <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]

# combine the flag with catch data and then group catch
Catch_Fishery <- cbind(Catch, Cell) %>%
  group_by(year,quarter,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

Catch_Fishery_plot <- cbind(Catch, Cell) %>%
  group_by(year,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

f4 <- ggplot(data=Catch_Fishery_plot) +
  geom_line(aes(x=year,y=Total_Catch,color=Cell)) +
  theme_bw()

library(patchwork)
(f1 + f2) / (f3 + f4)



### Longline

load("Data/LL_LF.RData")

LF <- LF_DF[,3:7] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(30, 170, 10),220),right = F,labels = seq(20, 170, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon) %>% data.frame()

# remove two locations
LF <- LF %>% filter(lon %in% c(32.5,52.5,72.5,92.5))

fcol <- 5 # the first column with LF_Tree info
lcol <- 20 # the last column with LF_Tree info
bins <- seq(20,170,10)
Nsplit <- 3 # the number of splits (the number of cells - 1)
save_dir <- "Data/"

# run the regression tree
LF_Tree <- run_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir)

# loop the regression tree for various combinations of splits
# library(RegressionTree)

LF_Tree_Loop <- loop_regression_tree(LF,fcol,lcol,Nsplit,save_dir,max_select = 2,quarter=FALSE)

# Regression Tree

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)

### Purse-seine
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_LF.RData")

LF <- LF_DF[,3:7] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(30, 140, 10),220),right = F,labels = seq(20, 140, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon) %>% data.frame()

# save(LF,file="LF.RData")

library(RegressionTree)

fcol <- 5 # the first column with LF_Tree info
lcol <- 17 # the last column with LF_Tree info
bins <- seq(20,140,10)
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

# LF_new include dummy data for catch allocation
LF_new <- rbind(LF,Catch_Grid)
LF_new$lat <- as.numeric(LF_new$lat)
LF_new$lon <- as.numeric(LF_new$lon)

# run the regression tree
# LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir, include_dummy = TRUE)
LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,max_select = 2)

select <- as.numeric(LF_Loop$Imp_DF[1,1:Nsplit])

LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)

# extract catch flag derived from the regression tree package
Catch_Flag <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]

# combine the flag with catch data and then group catch
Catch_Fishery <- cbind(Catch, Catch_Flag) %>%
  group_by(year,quarter,Catch_Flag) %>%
  summarise(Total_Catch=sum(Catch))


# 
# ### Longline
# 
# load("Data/LL_LF.RData")
# 
# LF <- LF_DF[,3:7] %>%
#   mutate(Length2=cut(Length,breaks = c(0,seq(50, 160, 10),220),right = F,labels = seq(40, 160, 10))) %>%
#   mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
#   group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
#   spread(Length2,LF) %>% rename(lat=Lat,lon=Lon)
# 
# fcol <- 5 # the first column with LF_Tree info
# lcol <- 17 # the last column with LF_Tree info
# Nsplit <- 3 # the number of splits (the number of cells - 1)
# save_dir <- "Data/"
# 
# # run the regression tree
# LF_Tree <- run_regression_tree(LF,fcol,lcol,Nsplit,save_dir)
# 
# # loop the regression tree for various combinations of splits
# # library(RegressionTree)
# 
# LF_Tree_Loop <- loop_regression_tree(LF,fcol,lcol,Nsplit,save_dir,max_select = 2,quarter=FALSE)

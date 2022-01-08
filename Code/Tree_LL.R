# Regression Tree for LL

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)
library(FishFreqTree)

### LL
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_LF.RData")

LF_raw <- LF_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  select(year,quarter,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon)

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
my_select_matrix <- data.matrix(expand.grid(
  select1 = 1:2,
  select2 = 1:2,
  select3 = 1:1
))
dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/")
save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/"

# load PS catch data
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_Catch.RData")
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

LF_new$lat[which(LF_new$dummy==FALSE)] <- LF_new$lat[which(LF_new$dummy==FALSE)] + 5
LF_new$lon[which(LF_new$dummy==FALSE)] <- LF_new$lon[which(LF_new$dummy==FALSE)] + 10

# run the regression tree
# LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir, include_dummy = TRUE)
LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix)
make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)

select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])

LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)
make.split.map(LF_Tree$LF,Nsplit,save_dir)

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

ggplot(data=Catch_Fishery_plot) +
  geom_line(aes(x=year,y=Total_Catch,color=Cell)) +
  theme_bw()

write.csv(Catch_Fishery,file=paste0(save_dir,"LL_Catch.csv"),row.names = FALSE)


# use the regression tree package to group lf

Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

Catch_cell <- cbind(Catch, Cell)
LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
  mutate(LF_raised = LF * Catch) %>%
  # mutate(LF_raised = LF) %>%
  select(year,quarter,lat,lon,Cell,Length,LF_raised)

# combine the flag with catch data and then group catch
LF_Fishery <- LF_raw_cell %>%
  group_by(Cell,year,quarter,Length) %>%
  summarise(lf_raised=sum(LF_raised)) %>%
  group_by(Cell,year,quarter) %>%
  mutate(lf_sum=sum(lf_raised)) %>%
  filter(lf_sum > 0) %>%
  mutate(lf=lf_raised/lf_sum) %>%
  select(Cell,year,quarter,Length,lf) %>%
  spread(Length,lf)

write.csv(LF_Fishery,file=paste0(save_dir,"LL_LF.csv"),row.names = FALSE)



# # catch-weighted tree
# 
# LF_weight <- left_join(LF,Catch) %>%
#   rename(weight=Catch) %>%
#   mutate(weight=weight/mean(weight))
# 
# LF_Loop <- loop_regression_tree(LF_weight,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix)
# make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)
# 
# select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])
# 
# Catch_Grid$weight <- 1
# LF_weight_new <- rbind(LF_weight,Catch_Grid)
# LF_weight_new$lat <- as.numeric(LF_weight_new$lat)
# LF_weight_new$lon <- as.numeric(LF_weight_new$lon)
# 
# LF_Tree <- run_regression_tree(LF_weight_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)
# 
# Cell <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]
# 
# # combine the flag with catch data and then group catch
# Catch_Fishery <- cbind(Catch, Cell) %>%
#   group_by(year,quarter,Cell) %>%
#   summarise(Total_Catch=sum(Catch)) %>%
#   mutate(Cell=factor(Cell))
# 
# Catch_Fishery_plot <- cbind(Catch, Cell) %>%
#   group_by(year,Cell) %>%
#   summarise(Total_Catch=sum(Catch)) %>%
#   mutate(Cell=factor(Cell))
# 
# ggplot(data=Catch_Fishery_plot) +
#   geom_line(aes(x=year,y=Total_Catch,color=Cell)) +
#   theme_bw()

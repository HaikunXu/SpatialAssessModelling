# Regression Tree for PS

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)

### Purse-seine
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_LF_0.RData")

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
Nsplit <- 2 # the number of splits (the number of cells - 1)
dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/")
save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/"

# run the regression tree
my_select_matrix <- data.matrix(expand.grid(
  select1 = 1:3,
  select2 = 1))

LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix, quarter = FALSE)
make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)


# load PS catch data
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_Catch_25.RData")
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

make.split.map(LF_Tree$LF,Nsplit,save_dir)
ggsave(file=paste0(save_dir,"Trees_0.png"), width = 10, height = 8)

# extract catch flag derived from the regression tree package
Cell <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]
# Cell[which(Cell>3)] <- 3

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
# 
# library(patchwork)

write.csv(Catch_Fishery,file=paste0(save_dir,"PS_Catch_0.csv"),row.names = FALSE)

# # use the regression tree package to group lf
# LF_Grid <- cbind(LF_raw[,1:4],matrix(0,nrow=nrow(LF),ncol=lcol-fcol+1))
# LF_Grid$dummy = TRUE
# names(LF_Grid) <- names(LF)

Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

Catch_cell <- cbind(Catch, Cell)

# sample weighted
LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
  # mutate(LF_raised = LF * Catch) %>% # catch weighted
  mutate(LF_raised = LF) %>%
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

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF_0.csv"),row.names = FALSE)


# catch weighted
LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
  mutate(LF_raised = LF * Catch) %>% # catch weighted
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

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF_0_cw.csv"),row.names = FALSE)



# # catch-weighted tree
# Nsplit <- 2 # the number of splits (the number of cells - 1)
# 
# # run the regression tree
# my_select_matrix <- data.matrix(expand.grid(
#   select1 = 1:2,
#   select2 = 1:1
# ))
# 
# LF_weight <- left_join(LF,Catch) %>%
#   rename(weight=Catch) %>%
#   mutate(weight=weight/mean(weight))
# 
# LF_Loop <- loop_regression_tree(LF_weight,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix, quarter = FALSE)
# make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)
# 
# select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])
# 
# Catch_Grid$weight <- 1
# LF_weight_new <- rbind(LF_weight,Catch_Grid)
# LF_weight_new$lat <- as.numeric(LF_weight_new$lat)
# LF_weight_new$lon <- as.numeric(LF_weight_new$lon)
# 
# LF_Tree <- run_regression_tree(LF_weight_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE, quarter = FALSE)
# 
# Cell <- LF_Tree$LF$Flag2[which(LF_Tree$LF$dummy == TRUE)]
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
# # #
# # # library(patchwork)
# # # ggsave((f1 + f2) / (f3 + f4), file=paste0(save_dir,"Trees.png"), width = 14, height = 12)
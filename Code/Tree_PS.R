# Regression Tree for PS

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)

### Purse-seine
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/PS_LF.RData")

LF_raw <- LF_DF %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  select(year,quarter,Lat,Lon,Length,LF) %>%
  rename(lat=Lat,lon=Lon)

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
Nsplit <- 2 # the number of splits (the number of cells - 1)
dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/")
save_dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/"

# run the regression tree
my_select_matrix <- data.matrix(expand.grid(
  select1 = 1:2,
  select2 = 1:1
  # select3 = 1:1
))

LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix)
make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)

select <- as.numeric(LF_Loop$Imp_DF_sorted[1,1:Nsplit])

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

# LF[6:8,fcol:lcol] <- LF[6:8,fcol:lcol] / 2 
# LF_new include dummy data for catch allocation
LF_new <- rbind(LF,Catch_Grid)
LF_new$lat <- as.numeric(LF_new$lat)
LF_new$lon <- as.numeric(LF_new$lon)

LF_Tree <- run_regression_tree(LF_new,fcol,lcol,bins,Nsplit,save_dir,manual=TRUE,select=select,include_dummy = TRUE)

# extract catch flag derived from the regression tree package
Cell <- LF_Tree$LF$Flag2[which(LF_Tree$LF$dummy == TRUE)]

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
# ggsave((f1 + f2), file=paste0(save_dir,"Trees.png"), width = 14, height = 12)

write.csv(Catch_Fishery,file=paste0(save_dir,"PS_Catch.csv"),row.names = FALSE)

# # use the regression tree package to group lf
# LF_Grid <- cbind(LF_raw[,1:4],matrix(0,nrow=nrow(LF),ncol=lcol-fcol+1))
# LF_Grid$dummy = TRUE
# names(LF_Grid) <- names(LF)

Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]

Catch_cell <- cbind(Catch, Cell)
LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
  # mutate(LF_raised = LF * Catch) %>%
  mutate(LF_raised = LF) %>%
  select(year,quarter,lat,lon,Cell,Length,LF_raised)

# combine the flag with catch data and then group catch
LF_Fishery <- LF_raw_cell %>%
  group_by(Cell,year,quarter,Length) %>%
  summarise(lf_raised=sum(LF_raised)) %>%
  group_by(Cell,year,quarter) %>%
  mutate(lf=lf_raised/sum(lf_raised)) %>%
  select(Cell,year,quarter,Length,lf) %>%
  spread(Length,lf)

write.csv(LF_Fishery,file=paste0(save_dir,"PS_LF.csv"),row.names = FALSE)





# # catch-weighted tree
# Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
# Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]
# 
# LF_weight <- left_join(LF,Catch) %>%
#   rename(weight=Catch) %>%
#   mutate(weight=weight/mean(weight))
# 
# LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix)
# f3 <- make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)
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
# f4 <- ggplot(data=Catch_Fishery_plot) +
#   geom_line(aes(x=year,y=Total_Catch,color=Cell)) +
#   theme_bw()
# # 
# # library(patchwork)
# ggsave((f1 + f2) / (f3 + f4), file=paste0(save_dir,"Trees.png"), width = 14, height = 12)
# 

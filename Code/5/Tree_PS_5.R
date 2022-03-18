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
  select1 = 1:3,
  select2 = 1:3,
  select3 = 1:1))

LF_Loop <- loop_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir,select_matrix = my_select_matrix, quarter = FALSE)

LF_Loop$LF_Tree$LF$Flag3 <- ifelse(LF_Loop$LF_Tree$LF$Flag3>3,3,LF_Loop$LF_Tree$LF$Flag3)

make.split.map(LF_Loop$LF_Tree$LF,Nsplit,save_dir)

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

N <- LF_Tree$LF %>% filter(dummy==FALSE) %>%
  mutate(Cell=ifelse(Flag3<3,Flag3,3)) %>%
  group_by(Cell,year,quarter) %>%
  summarise(n=length(unique(c(lat,lon))))

ggplot(data=N) +
  geom_point(aes(x=year,y=n,color=factor(Cell))) +
  facet_wrap(~quarter)

make.split.map(LF_Tree$LF,Nsplit,save_dir)
ggsave(file=paste0(save_dir,"Trees_5.png"), width = 10, height = 8)

# extract catch flag derived from the regression tree package
Cell <- LF_Tree$LF$Flag3[which(LF_Tree$LF$dummy == TRUE)]
Cell[which(Cell>3)] <- 3

# combine the flag with catch data and then group catch
Catch_Fishery <- cbind(Catch, Cell) %>%
  group_by(year,quarter,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

Catch_Fishery_plot <- cbind(Catch, Cell) %>%
  group_by(year,quarter,Cell) %>%
  summarise(Total_Catch=sum(Catch)) %>%
  mutate(Cell=factor(Cell))

ggplot(data=Catch_Fishery_plot) +
  geom_line(aes(x=year*4+quarter,y=Total_Catch,color=Cell)) +
  theme_bw()

ggsave(file=paste0(save_dir,"Trees_5_Catch.png"), width = 10, height = 8)

# 
# library(patchwork)

write.csv(Catch_Fishery,file=paste0(save_dir,"PS_Catch_5.csv"),row.names = FALSE)

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

write.csv(N,file=paste0(save_dir,"5_N.csv"),row.names = FALSE)

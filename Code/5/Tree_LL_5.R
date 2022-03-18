# Regression Tree for LL

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree")

library(tidyverse)
library(FishFreqTree)

### LL
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_LF_5.RData")

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
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_Catch_5.RData")
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

write.csv(N,file=paste0(save_dir,"5_N.csv"),row.names = FALSE)




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
# 
# write.csv(Catch_Fishery,file=paste0(save_dir,"LL_Catch_5.csv"),row.names = FALSE)


# # use the regression tree package to group lf
# 
# Catch$lat <- as.numeric(levels(Catch$lat))[Catch$lat]
# Catch$lon <- as.numeric(levels(Catch$lon))[Catch$lon]
# 
# Catch_cell <- cbind(Catch, Cell) %>%
#   mutate(lat=floor(lat/10)*10+7.5,
#          lon=floor(lon/20)*20+12.5)
# 
# # sample weighted LF
# LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
#   # mutate(LF_raised = LF * Catch) %>%
#   mutate(LF_raised = LF) %>%
#   select(year,quarter,lat,lon,Cell,Length,LF_raised) %>%
#   na.omit()
# 
# # ggplot() +
# #   geom_point(aes(x=lon,y=lat),shape=2,data=LF_raw) +
# #   geom_point(aes(x=lon,y=lat,color=factor(Cell)),shape=3,data=Catch_cell)
# 
# # ggplot() +
#   # geom_point(aes(x=lon,y=lat),shape=2,data=LF_raw) +
#   # geom_point(aes(x=lon,y=lat,color=factor(Cell)),data=LF_raw_cell)
# 
# # combine the flag with catch data and then group catch
# LF_Fishery <- LF_raw_cell %>%
#   group_by(Cell,year,quarter,Length) %>%
#   summarise(lf_raised=sum(LF_raised)) %>%
#   group_by(Cell,year,quarter) %>%
#   mutate(lf_sum=sum(lf_raised)) %>%
#   filter(lf_sum > 0) %>%
#   mutate(lf=lf_raised/lf_sum) %>%
#   select(Cell,year,quarter,Length,lf) %>%
#   spread(Length,lf)
# 
# write.csv(LF_Fishery,file=paste0(save_dir,"LL_LF_5.csv"),row.names = FALSE)
# 
# # f1 <- LF_Fishery
# 
# # catch weighted LF
# LF_raw_cell <- left_join(LF_raw,Catch_cell) %>%
#   mutate(LF_raised = LF * Catch) %>%
#   # mutate(LF_raised = LF) %>%
#   select(year,quarter,lat,lon,Cell,Length,LF_raised) %>%
#   na.omit()
# 
# # combine the flag with catch data and then group catch
# LF_Fishery <- LF_raw_cell %>%
#   group_by(Cell,year,quarter,Length) %>%
#   summarise(lf_raised=sum(LF_raised)) %>%
#   group_by(Cell,year,quarter) %>%
#   mutate(lf_sum=sum(lf_raised)) %>%
#   filter(lf_sum > 0) %>%
#   mutate(lf=lf_raised/lf_sum) %>%
#   select(Cell,year,quarter,Length,lf) %>%
#   spread(Length,lf)
# 
# write.csv(LF_Fishery,file=paste0(save_dir,"LL_LF_5_cw.csv"),row.names = FALSE)
# 
# f2 <- LF_Fishery
# 
# ggplot() +
#   geom_point(aes(x=year,y=quarter),data=f1) +
#   geom_point(aes(x=year,y=quarter),data=f2,color="red") +
#   facet_wrap(~Cell)

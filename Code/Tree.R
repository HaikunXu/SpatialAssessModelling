# Regression Tree

dir.create("Data/Tree")

library(tidyverse)

### Purse-seine
load("Data/PS_LF.RData")

LF <- LF_DF[,3:7] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(40, 150, 10),220),right = F,labels = seq(30, 150, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon)

# save(LF,file="LF.RData")

library(RegressionTree)

fcol <- 5 # the first column with LF_Tree info
lcol <- 15 # the last column with LF_Tree info
bins <- seq(30,150,10)
Nsplit <- 3 # the number of splits (the number of cells - 1)
dir.create("Data/Tree/PS")
save_dir <- "Data/Tree/PS"

# load PS catch data


# run the regression tree
LF_Tree <- run_regression_tree(LF,fcol,lcol,bins,Nsplit,save_dir)

### Longline

load("Data/LL_LF.RData")

LF <- LF_DF[,3:7] %>%
  mutate(Length2=cut(Length,breaks = c(0,seq(50, 160, 10),220),right = F,labels = seq(40, 160, 10))) %>%
  mutate(year=ceiling(Year/4),quarter=(Year-1)%%4 +1) %>%
  group_by(year,quarter,Lat,Lon,Length2) %>% summarise(LF=sum(LF)) %>%
  spread(Length2,LF) %>% rename(lat=Lat,lon=Lon)

fcol <- 5 # the first column with LF_Tree info
lcol <- 17 # the last column with LF_Tree info
Nsplit <- 3 # the number of splits (the number of cells - 1)
save_dir <- "Data/"

# run the regression tree
LF_Tree <- run_regression_tree(LF,fcol,lcol,Nsplit,save_dir)

# loop the regression tree for various combinations of splits
# library(RegressionTree)

LF_Tree_Loop <- loop_regression_tree(LF,fcol,lcol,Nsplit,save_dir,max_select = 2,quarter=FALSE)

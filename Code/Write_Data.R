library(r4ss)
library(tidyverse)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_4area_observations_1_100.RData")

data <- dat_4A_1

# Modify Fleet info
data$Nsurveys <- 1 
data$N_areas <- 1
data$fleetnames <- c(data$fleetnames[1:16],"llcpue")
data$surveytiming <- rep(0.5,17)
data$areas <- rep(1,17)

# Fleet info
fleetinfo1 <- cbind(data$fleetinfo1[,1:16],data.frame("llcpue"= c(0.5, 1)))
fleetinfo1[2,] <- 1
data$fleetinfo1 <- fleetinfo1

fleetinfo <- rbind(data$fleetinfo[1:16,],llcpue=c(0.5, 1, 3, 0))

# Survey CPUE
data$CPUEinfo <- data$CPUEinfo[1:17,]
CPUE <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Old/Data/VAST_Index/Table_for_SS3.csv")
CPUE$Fleet <- 17
CPUE$SD_log <- CPUE$SD_log + 0.2 - mean(CPUE$SD_log)
data$CPUE <- CPUE[,1:5]
data$N_cpue <- nrow(CPUE)

# Survey LF
LF0 <- data$lencomp
LL_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Old/Data/LL_LF_Nominal.csv")
LF <- rbind(data.matrix(LF0),data.matrix(LL_LF)) %>% data.frame()
names(LF) <- names(LF0)
data$lencomp <- LF
data$N_lencomp <- nrow(LF)

# Delete tagging data
data$do_tags <- 0

# Catch
Catch <- data$catch
PS_Catch <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/PS_Catch.csv")
PS_Catch <- PS_Catch %>%
  spread(Cell,Total_Catch)

Catch[,11:13] <- PS_Catch[3:5] / 1000
data$catch <- Catch

# LF
LF0 <- data$lencomp
PS_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/PS_LF.csv")
# remove old PS LF (fleet 11-13)
LF0 <- LF0[which(LF0$FltSvy %in% c(1:10,14:17)),]
# add new PS LF
PS_LF <- PS_LF %>%
  mutate(Yr=(year-1)*4+quarter,
         Seas=1,
         FltSvy=Cell+10,
         Gender=0,
         Part=0,
         Nsamp=5)
PS_LF_new <- PS_LF[,c(43:48,4:42)]

LF <- rbind(data.matrix(LF0),data.matrix(PS_LF_new)) %>% data.frame()
names(LF) <- names(LF0)
data$lencomp <- LF
data$N_lencomp <- nrow(LF)

# # write data file
library(r4ss)
SS_writedat(data,outfile = "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1/test_data.ss",version = "3.24",overwrite = TRUE)

# fix a bug in r4ss
File <- readLines("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1/test_data.ss", warn = F)
File[19] = "1 #_Nsexes"
writeLines(File, "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1/test_data.ss")
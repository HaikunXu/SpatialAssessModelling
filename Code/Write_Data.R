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
CPUE <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Spatial-SA-IATTC/Data/VAST_Index/Table_for_SS3.csv")
CPUE$Fleet <- 17
CPUE$SD_log <- CPUE$SD_log + 0.2 - mean(CPUE$SD_log)
data$CPUE <- CPUE[,1:5]
data$N_cpue <- nrow(CPUE)

# Survey LF
LF0 <- data$lencomp
LL_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Spatial-SA-IATTC/Data/LL_LF_Nominal.csv")
LF <- rbind(data.matrix(LF0),data.matrix(LL_LF)) %>% data.frame()
names(LF) <- names(LF0)
data$lencomp <- LF
data$N_lencomp <- nrow(LF)
# Delete tagging data
data$do_tags <- 0

# # write data file
library(r4ss)
SS_writedat(data,outfile = "Model/test_data.ss",version = "3.24",overwrite = TRUE)

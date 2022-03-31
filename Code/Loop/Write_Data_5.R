library(r4ss)
library(tidyverse)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_4area_observations_1_100_ESS_05.RData")
source("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Code/Loop/Loop.R")

for (i in 1:5) {
  # create a new folder for SS files
  loop_dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5/",i,"/")
  dir.create(loop_dir)
  
  Path <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5/"
  files = c(
    paste0(Path, "/go_nohess.bat"),
    paste0(Path, "/starter.ss"),
    paste0(Path, "/forecast.ss"),
    paste0(Path, "/YFT_IO_raw.ctl"),
    paste0(Path, "/test_data.ss"),
    paste0(Path, "/ss3.exe")
  )
  file.copy(from = files, to = loop_dir, overwrite = TRUE)
  
  # write data file
  data <- find_data(i)

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
CPUE <- read.csv(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_Index/Loop/",i,"/Table_for_SS3.csv"))
CPUE$Fleet <- 17
CPUE$SD_log <- CPUE$SD_log
data$CPUE <- CPUE[,1:5]
data$N_cpue <- nrow(CPUE)

# Delete tagging data
data$do_tags <- 0

# 
# LF
LF0 <- data$lencomp

N_LL <- read.csv(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/LL_5_N.csv"))
LF0$Nsamp[which(LF0$FltSvy %in% 4:7)] <- N_LL$n

N_PS <- read.csv(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/PS_5_N.csv"))
LF0$Nsamp[which(LF0$FltSvy %in% 11:13)] <- N_PS$n

data$lencomp <- LF0
data$N_lencomp <- nrow(LF0)

# # write data file
SS_writedat(data,outfile = paste0(loop_dir,"test_data.ss"),version = "3.24",overwrite = TRUE)

# fix a bug in r4ss
File <- readLines(paste0(loop_dir,"test_data.ss"), warn = F)
File[19] = "1 #_Nsexes"

# turn off comp tail compression
File[492] = "-1"
File[493] = "1e-9"

# Survey LF
SS_LF <- read.csv(file=paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/Loop/",i,"/SS.csv"))
# SS_LF$Nsamp <- 5
Line <- match("0 #_N_sizefreq_methods", File)
File[Line] = 1 # N WtFreq methods to read
File[Line+1] = 14 # nbins per method
File[Line+2] = 2 # units per each method
File[Line+3] = 3 # scale per each method
File[Line+4] = 0.000001 # mincomp to add to each obs (entry for each method)
File[Line+5] = nrow(SS_LF) # Number of observations per method 
File[Line+6] = paste0(gsub(", "," ",toString(c(-30,seq(40,160,10))))) # size bins
for (l in 1:nrow(SS_LF)) {
  File[Line+6+l] <- paste0(gsub(", "," ",toString(SS_LF[l,])))
}

File[Line+7+nrow(SS_LF)] = 0 # do tags
File[Line+8+nrow(SS_LF)] = 0 # no morphcomp data
File[Line+9+nrow(SS_LF)] = 999
File[Line+10+nrow(SS_LF)] = "ENDDATA"

writeLines(File, paste0(loop_dir,"test_data.ss"))

}
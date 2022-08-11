library(r4ss)
library(tidyverse)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_4area_observations_1_100_ESS_25.RData")

data <- dat_4A_4

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
CPUE <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_Index/VAST_Index_5/Table_for_SS3.csv")
CPUE$Fleet <- 17
CPUE$SD_log <- CPUE$SD_log
data$CPUE <- CPUE[,1:5]
data$N_cpue <- nrow(CPUE)

# Delete tagging data
data$do_tags <- 0

# 
# LF
LF0 <- data$lencomp
# remove old PS and LL LF (fleet 4-7, 11-13)
# LF0 <- LF0[which(LF0$FltSvy %in% c(1:3,8:10,14:17)),]
LF0 <- LF0[which(LF0$FltSvy %in% c(1:10,14:17)),]

N_LL <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/25_N.csv")
LF0$Nsamp[which(LF0$FltSvy %in% 4:7)] <- N_LL$n

PS_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/PS_LF_25_cw_nort.csv")

# new PS LF
PS_LF <- PS_LF %>%
  mutate(Yr=(year-1)*4+quarter,
         Seas=1,
         FltSvy=Cell+10,
         Gender=0,
         Part=0,
         Nsamp=n)
PS_LF_new <- PS_LF[,c(44:49,5:43)]

# add new PS anf LL LF
LF <- rbind(data.matrix(LF0),data.matrix(PS_LF_new)) %>% data.frame()
names(LF) <- names(LF0)
data$lencomp <- LF
data$N_lencomp <- nrow(LF)

# # write data file
SS_writedat(data,outfile = "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_25_cw/test_data.ss",version = "3.24",overwrite = TRUE)

# fix a bug in r4ss
File <- readLines("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_25_cw/test_data.ss", warn = F)
File[19] = "1 #_Nsexes"

# turn off comp tail compression
File[492] = "-1"
File[493] = "1e-9"

# Survey LF
SS_LF <- read.csv(file="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_25/SS.csv")
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

writeLines(File, "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_25_cw/test_data.ss")

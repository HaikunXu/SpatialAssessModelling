library(r4ss)
library(tidyverse)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_SRD_1A_4.RData")

data <- dat_1A_4

# Survey CPUE
CPUE <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_Index/VAST_Index_5/Table_for_SS3.csv")
CPUE$Fleet <- 8
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
# LF0 <- LF0[which(LF0$FltSvy %in% c(1:10,14:17)),]

N_LL <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/5_N_onearea.csv")
LF0$Nsamp[which(LF0$FltSvy %in% 3)] <- N_LL$n

N_PS <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/5_N_onearea.csv")

LF0$Nsamp[which(LF0$FltSvy %in% 6)] <- N_PS$n

data$lencomp <- LF0
data$N_lencomp <- nrow(LF0)

# # write data file
SS_writedat(data,outfile = "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_onearea/test_data.ss",version = "3.24",overwrite = TRUE)

# fix a bug in r4ss
File <- readLines("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_onearea/test_data.ss", warn = F)
File[19] = "1 #_Nsexes"

# # turn off comp tail compression
File[483] = "-1"
File[484] = "1e-9"

# Survey LF
SS_LF <- read.csv(file="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_5/SS.csv")
SS_LF$Flt <- 8
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

writeLines(File, "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_onearea/test_data.ss")


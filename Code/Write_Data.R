library(r4ss)
library(tidyverse)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_4area_observations_1_100_v2.RData")

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
CPUE <- read.csv("Data/VAST_Index/Table_for_SS3.csv")
CPUE$Fleet <- 17
CPUE$SD_log <- CPUE$SD_log + 0.2 - mean(CPUE$SD_log)
data$CPUE <- CPUE[,1:5]
data$N_cpue <- nrow(CPUE)

# LF0 <- data$lencomp
# LL_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Old/Data/LL_LF_Nominal.csv")
# LF <- rbind(data.matrix(LF0),data.matrix(LL_LF)) %>% data.frame()
# names(LF) <- names(LF0)
# data$lencomp <- LF
# # data$N_lencomp <- nrow(LF)

# Delete tagging data
data$do_tags <- 0

# # Catch
# Catch <- data$catch
# 
# PS_Catch <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/PS_Catch.csv")
# PS_Catch <- PS_Catch %>%
#   spread(Cell,Total_Catch)
# Catch[,11:13] <- PS_Catch[3:5] / 1000
# 
# # LL_Catch <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/LL_Catch.csv")
# # LL_Catch <- LL_Catch %>%
# #   spread(Cell,Total_Catch)
# # Catch[,4:7] <- LL_Catch[3:6] / 1000
# 
# data$catch <- Catch
# 
# # LF
# LF0 <- data$lencomp
# # remove old PS and LL LF (fleet 4-7, 11-13)
# # LF0 <- LF0[which(LF0$FltSvy %in% c(1:3,8:10,14:17)),]
# LF0 <- LF0[which(LF0$FltSvy %in% c(1:10,14:17)),]
# 
# PS_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/PS/PS_LF.csv")
# # new PS LF
# PS_LF <- PS_LF %>%
#   mutate(Yr=(year-1)*4+quarter,
#          Seas=1,
#          FltSvy=Cell+10,
#          Gender=0,
#          Part=0,
#          Nsamp=5)
# PS_LF_new <- PS_LF[,c(43:48,4:42)]
# 
# # LL_LF <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Tree/LL/LL_LF.csv")
# # # new LL LF
# # LL_LF <- LL_LF %>%
# #   mutate(Yr=(year-1)*4+quarter,
# #          Seas=1,
# #          FltSvy=Cell+3,
# #          Gender=0,
# #          Part=0,
# #          Nsamp=5)
# # LL_LF_new <- LL_LF[,c(43:48,4:42)]
# 
# # add new PS anf LL LF
# LF <- rbind(data.matrix(LF0),data.matrix(PS_LF_new)) %>% data.frame()
# names(LF) <- names(LF0)
# data$lencomp <- LF
# data$N_lencomp <- nrow(LF)


# # write data file
SS_writedat(data,outfile = "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1/test_data.ss",version = "3.24",overwrite = TRUE)

# fix a bug in r4ss
File <- readLines("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1/test_data.ss", warn = F)
File[19] = "1 #_Nsexes"

# Survey LF
SS_LF <- read.csv(file="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/SS.csv")
# SS_LF$Nsamp <- 5
Line <- match("0 #_N_sizefreq_methods", File)
File[Line] = 1 # N WtFreq methods to read
File[Line+1] = 13 # nbins per method
File[Line+2] = 2 # units per each method
File[Line+3] = 3 # scale per each method
File[Line+4] = 0.0001 # mincomp to add to each obs (entry for each method)
File[Line+5] = nrow(SS_LF) # Number of observations per method 
File[Line+6] = paste0(gsub(", "," ",toString(seq(40,160,10)))) # size bins
for (l in 1:nrow(SS_LF)) {
  File[Line+6+l] <- paste0(gsub(", "," ",toString(SS_LF[l,])))
}

File[Line+7+nrow(SS_LF)] = 0 # do tags
File[Line+8+nrow(SS_LF)] = 0 # no morphcomp data
File[Line+9+nrow(SS_LF)] = 999
File[Line+10+nrow(SS_LF)] = "ENDDATA"

writeLines(File, "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1/test_data.ss")

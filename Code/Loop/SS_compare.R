library(r4ss)
library(tidyverse)

### 5

N = 50

Path <- c("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_cw/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/")

Flag_all <- matrix(0,nrow=N,ncol=4)

for (i in c(1:46,48:50)) {
  for (j in 1:4) {
    load(paste0(Path[j],i,"/Flag.RData"))
    Flag_all[i,j] <- Flag
  }
}

Flag_all[47,] <- NA
# include <- which(apply(Flag_all,1,sum)==4)
Path_include <- c(paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5/",which(Flag_all[,1]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt/",which(Flag_all[,2]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_cw/",which(Flag_all[,3]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/",which(Flag_all[,4]==1)))

 
profilemodels <- SSgetoutput(dirvec = Path_include, getcovar = FALSE)

profilesummary <- SSsummarize(profilemodels[1:length(Path_include)])

Par <- profilesummary$pars

R0 <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "SR_LN(R0)"), 1:length(Path_include)])
# Q_extraSD <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "Q_extraSD_17_llcpue"), 1:36])

DF <- data.frame(
  R0 = R0,
  Model = c(
    rep("Model1", sum(Flag_all[, 1],na.rm = TRUE)),
    rep("Model2", sum(Flag_all[, 2],na.rm = TRUE)),
    rep("Model3", sum(Flag_all[, 3],na.rm = TRUE)),
    rep("Model4", sum(Flag_all[, 4],na.rm = TRUE))
  ),
  Replicate = c(
    which(Flag_all[, 1] == 1),
    which(Flag_all[, 2] == 1),
    which(Flag_all[, 3] == 1),
    which(Flag_all[, 4] == 1))
) %>%
  group_by(Replicate) %>% 
  mutate(Flag=length(unique(Model)))
  

Freq <- data.frame(
  Model = c("Model1", "Model2", "Model3", "Model4"),
  Converged = c(
    sum(Flag_all[, 1], na.rm = TRUE),
    sum(Flag_all[, 2], na.rm = TRUE),
    sum(Flag_all[, 3], na.rm = TRUE),
    sum(Flag_all[, 4], na.rm = TRUE)
  ),
  Not_Converged_SS = c(
    sum(Flag_all[, 1] == 0, na.rm = TRUE),
    sum(Flag_all[, 2] == 0, na.rm = TRUE),
    sum(Flag_all[, 3] == 0, na.rm = TRUE),
    sum(Flag_all[, 4] == 0, na.rm = TRUE)
  ),
  Not_Converged_VAST = c(sum(is.na(Flag_all[, 1])),
                          sum(is.na(Flag_all[, 2])),
                          sum(is.na(Flag_all[, 3])),
                          sum(is.na(Flag_all[, 4])))
) %>%
  gather(2:4, key = "Convergence", value = "Count")

f1 <- ggplot(data=Freq) +
  geom_col(aes(x=Model,y=Count,fill=Convergence)) +
  theme_bw() +
  theme(legend.position="bottom")

f2 <- ggplot(data=DF %>% filter(Flag==4)) +
  geom_boxplot(aes(x=Model,y=R0)) +
  # geom_boxplot(aes(x=Model,y=R0),width=0.1,color="red") +
  theme_bw(16) +
  ylab("ln(R0)") +
  geom_hline(yintercept = 11.48505)

library(patchwork)

f1 / f2


### 25
N = 10

Path <- c("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_rt/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_cw/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_rt_cw/")

Flag_all <- matrix(0,nrow=N,ncol=4)

for (i in c(1:N)) {
  for (j in 1:4) {
    load(paste0(Path[j],i,"/Flag.RData"))
    Flag_all[i,j] <- Flag
  }
}

# Flag_all[47,] <- NA
# include <- which(apply(Flag_all,1,sum)==4)
Path_include <- c(paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25/",which(Flag_all[,1]==1)),
                  paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_rt/",which(Flag_all[,2]==1)),
                  paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_cw/",which(Flag_all[,3]==1)),
                  paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_rt_cw/",which(Flag_all[,4]==1)))


profilemodels <- SSgetoutput(dirvec = Path_include, getcovar = FALSE)

profilesummary <- SSsummarize(profilemodels[1:length(Path_include)])

Par <- profilesummary$pars

R0 <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "SR_LN(R0)"), 1:length(Path_include)])
# Q_extraSD <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "Q_extraSD_17_llcpue"), 1:36])

DF <- data.frame(
  R0 = R0,
  Model = c(
    rep("Model1", sum(Flag_all[, 1],na.rm = TRUE)),
    rep("Model2", sum(Flag_all[, 2],na.rm = TRUE)),
    rep("Model3", sum(Flag_all[, 3],na.rm = TRUE)),
    rep("Model4", sum(Flag_all[, 4],na.rm = TRUE))
  ),
  Replicate = c(
    which(Flag_all[, 1] == 1),
    which(Flag_all[, 2] == 1),
    which(Flag_all[, 3] == 1),
    which(Flag_all[, 4] == 1))
) %>%
  group_by(Replicate) %>% 
  mutate(Flag=length(unique(Model)))


Freq <- data.frame(
  Model = c("Model1", "Model2", "Model3", "Model4"),
  Converged = c(
    sum(Flag_all[, 1], na.rm = TRUE),
    sum(Flag_all[, 2], na.rm = TRUE),
    sum(Flag_all[, 3], na.rm = TRUE),
    sum(Flag_all[, 4], na.rm = TRUE)
  ),
  Not_Converged_SS = c(
    sum(Flag_all[, 1] == 0, na.rm = TRUE),
    sum(Flag_all[, 2] == 0, na.rm = TRUE),
    sum(Flag_all[, 3] == 0, na.rm = TRUE),
    sum(Flag_all[, 4] == 0, na.rm = TRUE)
  ),
  Not_Converged_VAST = c(sum(is.na(Flag_all[, 1])),
                         sum(is.na(Flag_all[, 2])),
                         sum(is.na(Flag_all[, 3])),
                         sum(is.na(Flag_all[, 4])))
) %>%
  gather(2:4, key = "Convergence", value = "Count")

f1 <- ggplot(data=Freq) +
  geom_col(aes(x=Model,y=Count,fill=Convergence)) +
  theme_bw() +
  theme(legend.position="bottom")

f2 <- ggplot(data=DF %>% filter(Flag==4)) +
  geom_boxplot(aes(x=Model,y=R0)) +
  # geom_boxplot(aes(x=Model,y=R0),width=0.1,color="red") +
  theme_bw(16) +
  ylab("ln(R0)") +
  geom_hline(yintercept = 11.48505)

library(patchwork)

f1 / f2
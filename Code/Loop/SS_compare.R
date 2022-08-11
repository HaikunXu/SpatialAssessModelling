library(r4ss)
library(tidyverse)

### 5

<<<<<<< HEAD
N = 100
=======
N = 50
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f

Path <- c("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_cw/",
<<<<<<< HEAD
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_onearea/",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_onearea_cw/")

Flag_all <- matrix(0,nrow=N,ncol=6)

for (i in c(1:46,48:52,54:84,86:N)) {
  for (j in 1:6) {
=======
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/")

Flag_all <- matrix(0,nrow=N,ncol=4)

for (i in c(1:46,48:50)) {
  for (j in 1:4) {
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
    load(paste0(Path[j],i,"/Flag.RData"))
    Flag_all[i,j] <- Flag
  }
}

<<<<<<< HEAD
Flag_all[c(47,53,85),] <- NA
=======
Flag_all[47,] <- NA
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
# include <- which(apply(Flag_all,1,sum)==4)
Path_include <- c(paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5/",which(Flag_all[,1]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt/",which(Flag_all[,2]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_cw/",which(Flag_all[,3]==1)),
<<<<<<< HEAD
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/",which(Flag_all[,4]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_onearea/",which(Flag_all[,5]==1)),
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_onearea_cw/",which(Flag_all[,6]==1)))
=======
          paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/",which(Flag_all[,4]==1)))
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f

 
profilemodels <- SSgetoutput(dirvec = Path_include, getcovar = FALSE)

profilesummary <- SSsummarize(profilemodels[1:length(Path_include)])

Par <- profilesummary$pars

R0 <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "SR_LN(R0)"), 1:length(Path_include)])
# Q_extraSD <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "Q_extraSD_17_llcpue"), 1:36])

DF <- data.frame(
  R0 = R0,
  Model = c(
<<<<<<< HEAD
    rep("s-r", sum(Flag_all[, 1],na.rm = TRUE)),
    rep("s-t", sum(Flag_all[, 2],na.rm = TRUE)),
    rep("c-r", sum(Flag_all[, 3],na.rm = TRUE)),
    rep("c-t", sum(Flag_all[, 4],na.rm = TRUE)),
    rep("s-o", sum(Flag_all[, 5],na.rm = TRUE)),
    rep("c-o", sum(Flag_all[, 6],na.rm = TRUE))
=======
    rep("Model1", sum(Flag_all[, 1],na.rm = TRUE)),
    rep("Model2", sum(Flag_all[, 2],na.rm = TRUE)),
    rep("Model3", sum(Flag_all[, 3],na.rm = TRUE)),
    rep("Model4", sum(Flag_all[, 4],na.rm = TRUE))
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  ),
  Replicate = c(
    which(Flag_all[, 1] == 1),
    which(Flag_all[, 2] == 1),
    which(Flag_all[, 3] == 1),
<<<<<<< HEAD
    which(Flag_all[, 4] == 1),
    which(Flag_all[, 5] == 1),
    which(Flag_all[, 6] == 1))
  ) %>%
=======
    which(Flag_all[, 4] == 1))
) %>%
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  group_by(Replicate) %>% 
  mutate(Flag=length(unique(Model)))
  

Freq <- data.frame(
<<<<<<< HEAD
  Model = c("s-r", "s-t", "c-r", "c-t", "s-o", "c-o"),
=======
  Model = c("Model1", "Model2", "Model3", "Model4"),
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  Converged = c(
    sum(Flag_all[, 1], na.rm = TRUE),
    sum(Flag_all[, 2], na.rm = TRUE),
    sum(Flag_all[, 3], na.rm = TRUE),
<<<<<<< HEAD
    sum(Flag_all[, 4], na.rm = TRUE),
    sum(Flag_all[, 5], na.rm = TRUE),
    sum(Flag_all[, 6], na.rm = TRUE)
=======
    sum(Flag_all[, 4], na.rm = TRUE)
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  ),
  Not_Converged_SS = c(
    sum(Flag_all[, 1] == 0, na.rm = TRUE),
    sum(Flag_all[, 2] == 0, na.rm = TRUE),
    sum(Flag_all[, 3] == 0, na.rm = TRUE),
<<<<<<< HEAD
    sum(Flag_all[, 4] == 0, na.rm = TRUE),
    sum(Flag_all[, 5] == 0, na.rm = TRUE),
    sum(Flag_all[, 6] == 0, na.rm = TRUE)
  ),
  Not_Converged_VAST = c(sum(is.na(Flag_all[, 1])),
                         sum(is.na(Flag_all[, 2])),
                         sum(is.na(Flag_all[, 3])),
                         sum(is.na(Flag_all[, 4])),
                         sum(is.na(Flag_all[, 5])),
                         sum(is.na(Flag_all[, 6])))
=======
    sum(Flag_all[, 4] == 0, na.rm = TRUE)
  ),
  Not_Converged_VAST = c(sum(is.na(Flag_all[, 1])),
                          sum(is.na(Flag_all[, 2])),
                          sum(is.na(Flag_all[, 3])),
                          sum(is.na(Flag_all[, 4])))
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
) %>%
  gather(2:4, key = "Convergence", value = "Count")

f1 <- ggplot(data=Freq) +
  geom_col(aes(x=Model,y=Count,fill=Convergence)) +
<<<<<<< HEAD
  theme_bw(16) +
  theme(legend.position="bottom")

f2 <- ggplot(data=DF) +
  geom_boxplot(aes(x=Model,y=R0)) +
  # geom_boxplot(aes(x=Model,y=R0),width=0.1,color="red") +
  theme_bw(16) +
  ylim(c(11.1,12.1))+
=======
  theme_bw() +
  theme(legend.position="bottom")

f2 <- ggplot(data=DF %>% filter(Flag==4)) +
  geom_boxplot(aes(x=Model,y=R0)) +
  # geom_boxplot(aes(x=Model,y=R0),width=0.1,color="red") +
  theme_bw(16) +
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  ylab("ln(R0)") +
  geom_hline(yintercept = 11.48505)

library(patchwork)

f1 / f2

ggsave(f1 / f2, file="C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/5.png",width=10,height=8)
save.image(file="C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/5.RData")

### 25
<<<<<<< HEAD
N = 100
=======
N = 10
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f

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
<<<<<<< HEAD

Par <- profilesummary$pars

R0 <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "SR_LN(R0)"), 1:length(Path_include)])
# Q_extraSD <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "Q_extraSD_17_llcpue"), 1:36])

DF <- data.frame(
  R0 = R0,
  Model = c(
    rep("s-r", sum(Flag_all[, 1],na.rm = TRUE)),
    rep("s-t", sum(Flag_all[, 2],na.rm = TRUE)),
    rep("c-r", sum(Flag_all[, 3],na.rm = TRUE)),
    rep("c-t", sum(Flag_all[, 4],na.rm = TRUE))
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
  Model = c("s-r", "s-t", "c-r", "c-t"),
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
  theme_bw(16) +
  theme(legend.position="bottom")

f2 <- ggplot(data=DF) + # %>% filter(Flag==4)) +
=======

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
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  geom_boxplot(aes(x=Model,y=R0)) +
  # geom_boxplot(aes(x=Model,y=R0),width=0.1,color="red") +
  theme_bw(16) +
  ylab("ln(R0)") +
<<<<<<< HEAD
  ylim(c(11.1,12.1))+
=======
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f
  geom_hline(yintercept = 11.48505)

library(patchwork)

<<<<<<< HEAD
f1 / f2

ggsave(f1 / f2, file="C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/25.png",width=8,height=8)
save.image(file="C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/25.RData")
=======
f1 / f2
>>>>>>> aa4dc4d7a6056112e6582ed7753907d0a0aa348f

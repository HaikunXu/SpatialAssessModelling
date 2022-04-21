library(r4ss)
library(tidyverse)

Path <- c(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5/",c(1,3:7)),
          paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt/",c(1,3:7)),
          paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_5_rt_cw/",c(1,3:7)),
          paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25/",c(1,3:7)),
          paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_rt/",c(1,3:7)),
          paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_rt_cw/",c(1,3:7)))

profilemodels <- SSgetoutput(dirvec = Path, getcovar = FALSE)

profilesummary <- SSsummarize(profilemodels[1:36])

R0 <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "SR_LN(R0)"), 1:36])
Q_extraSD <- as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "Q_extraSD_17_llcpue"), 1:36])

DF <- data.frame(R0=R0,
                    replicate=c(1,3:7),
                    Model=rep(c("Model1","Model2","Model3"),each=6),
                    ESS=rep(c(5,25),each=18),
                    Q_extraSD=Q_extraSD)

ggplot(data=DF) +
  geom_boxplot(aes(x=Model,y=R0,fill=factor(ESS))) +
  theme_bw() +
  # facet_wrap(~ESS) +
  geom_hline(yintercept = 11.48505)

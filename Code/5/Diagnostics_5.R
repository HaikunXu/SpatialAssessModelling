library(r4ss)
library(IATTCassessment)
library(ss3diags)

dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt_cw/"
setwd(dir)

Rep = SS_output(dir=dir,ncols=400,covar=T,verbose = FALSE, printstats = FALSE)

# runs_test
SSplotRunstest(Rep, add = T, verbose = F)
ggsave(file = "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Index.png", width = 6, height = 6)

sspar(mfrow = c(4, 4), plot.cex = 0.8)
SSplotRunstest(Rep, add = T, subplot = "len", verbose = F)

# R0 profile
Path <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/R0/"
R0 <- seq(10.75,12.25,0.3)

N = length(R0)
comps <- c(1:17)
NLL_a <- data.frame("Total"=rep(NA,N),
                    "Index"=rep(NA,N),
                    "Fcomps"=rep(NA,N),
                    "Scomps"=rep(NA,N),
                    "Recruit"=rep(NA,N),
                    "R0"=R0)

NLL_comp <- data.frame("R0"=R0)
for(f in 1:length(comps)) NLL_comp <- cbind(NLL_comp, rep(NA,N))

for (n in 1:N) {
  myreplist = r4ss::SS_output(dir = paste0(Path,toString(R0[n])),ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
  NLL_a[n,1:5] <- myreplist$likelihoods_used$values[c(1,4,5,6,7)]
  
  NLL_temp <- myreplist$likelihoods_by_fleet
  for(c in 1:length(comps)) {
    if(comps[c]<17) NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="Length_like",comps[c]+2]
    else NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="SizeFreq_like:_1",comps[c]+2]
  }
}

names(NLL_comp)[2:(length(comps)+1)] <- names(NLL_temp)[comps+2]
NLL_amin <- NLL_a %>% gather(Total,Index,Fcomps,Scomps,Recruit,value="nll",key="Component") %>%
  group_by(Component) %>% mutate(NLL=nll-min(nll))

ggplot() +
  geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=1) +
  geom_point(aes(x=R0,y=NLL,color=Component,shape=Component),data=NLL_amin,size=3) +
  theme_bw(12) +
  # geom_vline(xintercept = 11.48505,linetype="dashed") +
  xlab("ln(R0)") +
  ylab("NLL")

ggsave(file = paste0(Path, "R0.png"), width = 8, height = 6)


# ASPM
Path1 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt_cw/"
Rep1 = SS_output(dir=Path1,ncols=400,covar=T)

Path2 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/ASPM/"
Rep2 = SS_output(dir=Path2,ncols=400,covar=T)

Path3 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/ASPM - R/"
Rep3 = SS_output(dir=Path3,ncols=400,covar=T)

# Path4 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/CCA/"
# Rep4 = SS_output(dir=Path4,ncols=400,covar=F)

SSplotComparisons(SSsummarize(list(Rep1,Rep2,Rep3)), subplot=1:50,
                  legendlabels=c("Model3","ASPM","ASPM_Rdev"), btarg=0, minbthresh=0,
                  plotdir = 'D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/ASPM/', print = TRUE)


# retro
Path1 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/0/"
Rep1 = SS_output(dir=Path1,ncols=400,covar=F)

Path2 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/4/"
Rep2 = SS_output(dir=Path2,ncols=400,covar=F)

Path3 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/8/"
Rep3 = SS_output(dir=Path3,ncols=400,covar=F)

Path4 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/12/"
Rep4 = SS_output(dir=Path4,ncols=400,covar=F)

Path5 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/16/"
Rep5 = SS_output(dir=Path5,ncols=400,covar=F)

Path6 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/20/"
Rep6 = SS_output(dir=Path6,ncols=400,covar=F)

retroSummary <- SSsummarize(list(Rep1,Rep2,Rep3,Rep4,Rep5,Rep6))
endyrvec <- retroSummary$endyrs + seq(0,-20,-4)

SSplotComparisons(retroSummary, endyrvec=endyrvec, legendlabels=paste("Data",seq(0,-20,-4),"years"),
                  plot = FALSE, print = TRUE, plotdir = "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Retro/")


### selectivity
replist <- r4ss::SS_output(dir = dir, ncols = 400, covar = T, printstats = F, verbose = FALSE)
startYr <- replist$startyr
endYr <- replist$endyr
numSexes <- replist$nsexes
numFleets <- replist$nfleets
SizeSelexDat <- replist$ageselex
numSizeBins <- replist$nlbins
SizeBins <- replist$lbinspop
FleetNames <- replist$FleetNames

FleetNums <- 1:16

SizeSelex <- SizeSelexDat %>% filter(Yr %in% c(startYr),
                                     Fleet %in% FleetNums,
                                     Factor=="Asel")
SizeSelex$FLeet_Names <- FleetNames[SizeSelex$Fleet]

SizeSelex_DF <- SizeSelex %>% gather(8:(ncol(SizeSelex)-1),key="Age",value="Selectivity") %>%
  mutate(Age=as.numeric(Age))

ggplot(data=SizeSelex_DF) +
  # geom_line(aes(x=Length,y=Selectivity,color=Time)) +
  geom_line(aes(x=Age,y=Selectivity)) +
  facet_wrap(~FLeet_Names) +
  theme_bw(15)

ggsave(file = paste0(dir, "selex.png"), width = 10, height = 10)

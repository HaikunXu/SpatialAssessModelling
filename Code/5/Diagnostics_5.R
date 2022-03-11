library(r4ss)
library(IATTCassessment)
library(ss3diags)

dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5/"
setwd(dir)

Rep = SS_output(dir=dir,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)

#runs_test
# rt = SSplotRunstest(Rep, add = T, legendcex = 0.8, verbose = F)

Path <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/R0/"
R0 <- seq(10.85,12.05,0.3)

N = length(R0)
comps <- c(1:17)
NLL_a <- data.frame("Total"=rep(NA,N),
                    "Indices"=rep(NA,N),
                    "Fcomps"=rep(NA,N),
                    "Scomps"=rep(NA,N),
                    "Recruits"=rep(NA,N),
                    "R0"=R0)

NLL_comp <- data.frame("R0"=R0)
for(f in 1:length(comps)) NLL_comp <- cbind(NLL_comp, rep(NA,N))

for (n in 1:N) {
  myreplist = r4ss::SS_output(dir = paste0(Path,toString(R0[n])),ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
  NLL_a[n,1:5] <- myreplist$likelihoods_used$values[c(1,4,5,6,7)]
  
  NLL_temp <- myreplist$likelihoods_by_fleet
  for(c in 1:length(comps)) {
    if(comps[c]<24) NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="Length_like",comps[c]+2]
    else NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="SizeFreq_like:_1",comps[c]+2]
  }
}

names(NLL_comp)[2:(length(comps)+1)] <- names(NLL_temp)[comps+2]
NLL_amin <- NLL_a %>% gather(Total,Indices,Fcomps,Scomps,Recruits,value="nll",key="Component") %>%
  group_by(Component) %>% mutate(NLL=nll-min(nll))

ggplot() +
  geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=1) +
  geom_point(aes(x=R0,y=NLL,color=Component,shape=Component),data=NLL_amin,size=3) +
  theme_bw(20) +
  xlab("") +
  ylab("")

# ggsave(f_all, file = paste0(Path, "R0.png"), width = 6, height = 6)


# ASPM
Path1 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt_cw/"
Rep1 = SS_output(dir=Path1,ncols=400,covar=T)

Path2 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/ASPM/"
Rep2 = SS_output(dir=Path2,ncols=400,covar=T)

Path3 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/ASPM - R/"
Rep3 = SS_output(dir=Path3,ncols=400,covar=T)

Path4 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/CCA/"
Rep4 = SS_output(dir=Path4,ncols=400,covar=T)

SSplotComparisons(SSsummarize(list(Rep1,Rep2,Rep3,Rep4)), subplot=1:50,
                  legendlabels=c("Model3","ASPM","ASPM_Rdev","CCA"), btarg=0, minbthresh=0,
                  plotdir = 'D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/ASPM/', print = TRUE)

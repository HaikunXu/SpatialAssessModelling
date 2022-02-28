library(r4ss)

Path1 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_25/"
Rep1 = SS_output(dir=Path1,ncols=400,covar=T)

Path2 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_25_rt/"
Rep2 = SS_output(dir=Path2,ncols=400,covar=T)

SSplotComparisons(SSsummarize(list(Rep1,Rep2)), subplot=1:50,
                  legendlabels=c("Model1","Model2"), btarg=0, minbthresh=0,
                  plotdir = 'D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/compare/', print = TRUE)

library(r4ss)

Path1 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5/"
Rep1 = SS_output(dir=Path1,ncols=400,covar=T)

Path2 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt/"
Rep2 = SS_output(dir=Path2,ncols=400,covar=T)

Path3 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt_cw/"
Rep3 = SS_output(dir=Path3,ncols=400,covar=T)

Path4 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_cw/"
Rep4 = SS_output(dir=Path4,ncols=400,covar=T)

Path5 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_onearea/"
Rep5 = SS_output(dir=Path5,ncols=400,covar=T)

Path6 <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_onearea_cw/"
Rep6 = SS_output(dir=Path6,ncols=400,covar=T)

SSplotComparisons(SSsummarize(list(Rep1,Rep2,Rep4,Rep3,Rep5,Rep6)), subplot=1:50,
                  legendlabels=c("sample-region","sample-tree","catch-region",
                                 "catch-tree","sample-one","catch-one"), 
                  btarg=0, minbthresh=0,
                  plotdir = 'D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/compare/', print = TRUE)

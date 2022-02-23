library(VAST)
library(tidyverse)

load("Data/CPUE_25.RData")
Data_Geostat$Vessel <- "NA"
Data_Geostat$AreaSwept_km2 <- 1

CPUE <- Data_Geostat %>%
  group_by(Year) %>%
  summarise(cpue=mean(Catch_KG))

ggplot(data=CPUE) +
  geom_line(aes(x=Year,y=cpue)) +
  theme_bw()

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_1area_observations_1_100_ESS_25.RData")
plot(dat_1A_1$CPUE$cpu)

write.csv(dat_1A_1$CPUE,file="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_Index/VAST_Index_25/CPUE.csv",row.names = FALSE)

settings = make_settings( n_x=150, Region="Other", purpose="index2",max_cells=Inf,use_anisotropy=FALSE,
                          strata.limits=data.frame('STRATA'=c("IO")), bias.correct=FALSE, ObsModel=c(1,3),
                          RhoConfig=c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) )


dir <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_Index/VAST_Index_25/"
dir.create(dir)
setwd(dir)

fit_yft = fit_model( "settings"=settings, "Lat_i"=Data_Geostat[,'Lat'],
                     "Lon_i"=Data_Geostat[,'Lon'], "t_i"=Data_Geostat[,'Year'],
                     "c_i"=rep(0,nrow(Data_Geostat)), "b_i"=Data_Geostat[,'Catch_KG'],
                     "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=Data_Geostat[,'Vessel'],
                     "working_dir"=dir,
                     "grid_dim_km" = c(50,50),
                     "observations_LL"=Data_Geostat[,c('Lat','Lon')])

# save(fit_orig, file=paste0(working_dir,"Sim1/fit_orig.RData"))
Results = plot_results(settings=settings, fit=fit_yft)
plot(fit_yft)
save.image(file="all.RData")

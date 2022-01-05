library(VAST)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/CPUE.RData")
Data_Geostat$Vessel <- "NA"
Data_Geostat$AreaSwept_km2 <- 1

settings = make_settings( n_x=150, Region="Other", purpose="index2",max_cells=Inf,use_anisotropy=FALSE,
                          strata.limits=data.frame('STRATA'=c("IO")), bias.correct=FALSE, ObsModel=c(1,3),
                          RhoConfig=c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) )

dir.create("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/VAST_Index/")
setwd("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/VAST_Index/")

fit_yft = fit_model( "settings"=settings, "Lat_i"=Data_Geostat[,'Lat'],
                     "Lon_i"=Data_Geostat[,'Lon'], "t_i"=Data_Geostat[,'Year'],
                     "c_i"=rep(0,nrow(Data_Geostat)), "b_i"=Data_Geostat[,'Catch_KG'],
                     "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=Data_Geostat[,'Vessel'],
                     "working_dir"="D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/VAST_Index/",
                     "grid_dim_km" = c(50,50),
                     "observations_LL"=Data_Geostat[,c('Lat','Lon')])

# save(fit_orig, file=paste0(working_dir,"Sim1/fit_orig.RData"))
plot(fit_yft)
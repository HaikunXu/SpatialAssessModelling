library(VAST)
library(tidyverse)

for (i in 11:100) {
  
  load(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",toString(i),"/CPUE_5.RData"))

Data_Geostat$Vessel <- "NA"
Data_Geostat$AreaSwept_km2 <- 1

settings = make_settings( n_x=150, Region="Other", purpose="index2",max_cells=Inf,use_anisotropy=FALSE,
                          strata.limits=data.frame('STRATA'=c("IO")), bias.correct=FALSE, ObsModel=c(1,3),
                          RhoConfig=c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0),
                          fine_scale = FALSE)

dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_Index/Loop/",toString(i),"/")
dir.create(dir)  
setwd(dir) 

skip_to_next <- FALSE

tryCatch({
  fit = fit_model(
    settings = settings,
    Lat_i = Data_Geostat[, 'Lat'],
    Lon_i = Data_Geostat[, 'Lon'],
    t_i = Data_Geostat[, 'Year'],
    c_i = rep(0, nrow(Data_Geostat)),
    b_i = Data_Geostat[, 'Catch_KG'],
    a_i = Data_Geostat[, 'AreaSwept_km2'],
    v_i = Data_Geostat[, 'Vessel'],
    working_dir = dir,
    grid_dim_km = c(50, 50),
    newtonsteps = 1,
    test_fit = FALSE,
    observations_LL = Data_Geostat[, c('Lat', 'Lon')]
  )
  
  Results = plot_results(settings=settings, fit=fit)
  
  }, error = function(e) {skip_to_next <<- TRUE})
if (skip_to_next) {next}

}
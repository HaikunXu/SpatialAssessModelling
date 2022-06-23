library(VAST)
library(tidyverse)

for (i in 53:100) {
  
  load(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",toString(i),"/CPUE_25.RData"))
  load(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",toString(i),"/LL_LF_25.RData"))
  
  LF_DF <- LF_DF %>% group_by(Year,Lat,Lon) %>%
    mutate(tot=sum(LF)) %>% filter(tot>0)
  
  Data_Geostat2 <- Data_Geostat %>%
    mutate(Lat=floor(Lat/10)*10+7.5,
           Lon=floor(Lon/20)*20+12.5) %>%
    group_by(Year,Lat,Lon) %>%
    summarise(Catch_KG=mean(Catch_KG))
  
  Data <- left_join(LF_DF,Data_Geostat2) %>%
    rename(Catch_KG2=Catch_KG) %>%
    mutate(Catch_KG=Catch_KG2*LF) %>% na.omit() %>%
    # filter(Lon %in% c(37.5,102.5) == FALSE) %>%
    mutate(spp=ifelse(Length>160,160,
                      ifelse(Length<30,30,floor(Length/10)*10))) %>%
    group_by(Year,Lat,Lon,spp) %>% summarise(Catch_KG=sum(Catch_KG)) %>%
    data.frame()
  
  Data$Vessel <- "NA"
  Data$AreaSwept_km2 <- 1
  
  # Data <- Data %>% mutate(spp=ifelse(spp>30,spp,30))
  
  settings = make_settings( n_x=10, Region="Other", purpose="index2",max_cells=Inf,use_anisotropy=FALSE,
                            strata.limits=data.frame('STRATA'=c("IO")), bias.correct=FALSE, ObsModel=c(1,3),
                            fine_scale = FALSE)
  
  # settings$ObsModel = c(1,3)
  # settings$use_anisotropy = FALSE
  settings$Options[['treat_nonencounter_as_zero']] = TRUE
  
  dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/Loop-Copy/",toString(i),"/")
  dir.create(dir)  
  setwd(dir) 
  
  Nsamp <- left_join(LF_DF,Data_Geostat2) %>%
    rename(Catch_KG2=Catch_KG) %>%
    mutate(Catch_KG=Catch_KG2*LF) %>% na.omit() %>%
    # filter(Lon %in% c(37.5,102.5) == FALSE) %>%
    # mutate(LL=(lat,lon)) %>%
    # mutate(spp=Length) %>%
    group_by(Year) %>% summarise(N=length(unique(paste0(lat,lon)))) %>%
    data.frame()
  
  write.csv(Nsamp,file=paste0(dir,"Nsamp.csv"),row.names = FALSE)
  
  skip_to_next <- FALSE
  
  tryCatch({
    fit = fit_model( settings = settings,
                     Lat_i = Data[,'Lat'],
                     Lon_i = Data[,'Lon'],
                     t_i = Data[,'Year'],
                     c_i = as.numeric(factor(Data[,'spp']))-1,
                     b_i = Data[,'Catch_KG'],
                     a_i = Data[,'AreaSwept_km2'],
                     Npool = 1000000,
                     newtonsteps = 2,
                     test_fit = FALSE,
                     working_dir=dir,
                     grid_dim_km = c(50,50),
                     observations_LL=Data[,c('Lat','Lon')])
    
    Results = plot_results(settings=settings, fit=fit)
    save.image(file="all.RData")
    
  }, error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next) {next}
  
  # if(fit$parameter_estimates$max_gradient<0.1) {
  
  # }
  
}

library(VAST)
library(tidyverse)

load("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/CPUE_5.RData")
load("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_LF_5.RData")

LF_DF <- LF_DF %>% group_by(Year,Lat,Lon) %>%
  mutate(tot=sum(LF)) %>% filter(tot>0)

Data <- left_join(LF_DF,Data_Geostat) %>%
  rename(Catch_KG2=Catch_KG) %>%
  mutate(Catch_KG=Catch_KG2*LF) %>% na.omit() %>%
  # filter(Lon %in% c(37.5,102.5) == FALSE) %>%
  mutate(spp=ifelse(Length>180,180,floor(Length/10)*10)) %>%
  # mutate(spp=Length) %>%
  group_by(Year,Lat,Lon,spp) %>% summarise(Catch_KG=sum(Catch_KG)) %>%
  data.frame()

ggplot(Data) +
  geom_point(aes(x=Year,y=spp,color=Catch_KG>0))

# ggplot(Nsamp) +
#   geom_point(aes(x=Lon,y=Lat)) +
#   facet_wrap(~Year)

Data_all <- Data %>%
  group_by(Year,spp) %>%
  summarise(CPUE=mean(Catch_KG))

# ggplot(Data_all %>% filter(spp==80)) +
#   geom_point(aes(x=Year,y=CPUE))

Data$Vessel <- "NA"
Data$AreaSwept_km2 <- 1

Data <- Data %>% filter(spp>10)

settings = make_settings( n_x=10, Region="Other", purpose="index2",max_cells=Inf,use_anisotropy=FALSE,
                          strata.limits=data.frame('STRATA'=c("IO")), bias.correct=FALSE, ObsModel=c(1,3),
                          fine_scale = FALSE)

# settings$ObsModel = c(1,3)
# settings$use_anisotropy = FALSE
settings$Options[['treat_nonencounter_as_zero']] = TRUE

dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_5/"
dir.create(dir)
setwd(dir)

Nsamp <- left_join(LF_DF,Data_Geostat) %>%
  rename(Catch_KG2=Catch_KG) %>%
  mutate(Catch_KG=Catch_KG2*LF) %>% na.omit() %>%
  # filter(Lon %in% c(37.5,102.5) == FALSE) %>%
  # mutate(LL=(lat,lon)) %>%
  # mutate(spp=Length) %>%
  group_by(Year) %>% summarise(N=length(unique(paste0(lat,lon)))) %>%
  data.frame()

write.csv(Nsamp,file=paste0(dir,"Nsamp.csv"),row.names = FALSE)

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
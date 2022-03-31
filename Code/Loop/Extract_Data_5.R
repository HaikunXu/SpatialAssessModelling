library(tidyverse)

load("Data/sim_1.RData")
# load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/25/sim_i.RData")

Lat_grid <- data.frame("lat" = paste0("r",as.character(seq(1,13))),
                       "Lat" = sim_1$layers$`layer[latitude]`$data[,1])
Lon_grid <- data.frame("lon" = paste0("c",as.character(seq(1,17))),
                       "Lon" = sim_1$layers$`layer[longitude]`$data[1,])

rm(sim_1)

load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_221cell_observations_1-100_ESS_05.RData")
source("Code/Loop/Loop.R")

for (i in 1:10) {

  loop_dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",toString(i))
dir.create(loop_dir)  
setwd(loop_dir) 

sim_i <- find_sim(i)

# LL CPUE data
for (year in 81:256) {
  CPUE <- sim_i$obs[[paste0("simulated_cpue_ll_jpn_",year)]]
  
  if(year==81) Data_Geostat0 <- data.frame("Year"=year,"LatLon"=CPUE$data$obs[,1],"Catch_KG"=CPUE$data$obs[,2])
  else Data_Geostat0 <- rbind(Data_Geostat0,
                              data.frame("Year"=year,"LatLon"=CPUE$data$obs[,1],"Catch_KG"=CPUE$data$obs[,2]))                                   
}

Data_Geostat <- Data_Geostat0 %>%
  mutate(Catch_KG=ifelse(as.numeric(levels(Catch_KG))[Catch_KG]<1e-6,0,as.numeric(levels(Catch_KG))[Catch_KG])) %>%
  separate(LatLon, c("lat", "lon"), "-")

Data_Geostat <- left_join(left_join(Data_Geostat,Lat_grid),Lon_grid) %>%
  mutate(Lat=as.numeric(levels(Lat))[Lat],Lon=as.numeric(levels(Lon))[Lon])

save(Data_Geostat,file="CPUE_5.RData")

# LL LF data
for (year in 9:238) {
  # year=210
  LF <- sim_i$obs[[paste0("simulated_lf_ll_",year)]]
  
  if(length(LF$data$obs)>40) LF_DF_year <- data.frame(LF$data$obs) %>% mutate(Year=year) # more than 1 obs
  if(length(LF$data$obs)==40) LF_DF_year <- data.frame(t(LF$data$obs)) %>% mutate(Year=year) # 1 obs
  
  if(year==9) LF_DF0 <- LF_DF_year
  else LF_DF0 <- rbind(LF_DF0,LF_DF_year)                                 
}

names(LF_DF0)[1] <- "LatLon"
names(LF_DF0)[2:40] <- LF$data$length_bins[1:39]

# LF_DF0 <- unique(LF_DF0)

LF_DF <- LF_DF0 %>% gather(2:40,key="Length",value = "LF") %>%
  mutate(Length=as.numeric(Length),LF=as.numeric(LF)) %>%
  separate(LatLon, c("lat", "lon"), "-")

LF_DF <- left_join(left_join(LF_DF,Lat_grid),Lon_grid) %>%
  mutate(Lat=as.numeric(levels(Lat))[Lat],Lon=as.numeric(levels(Lon))[Lon])

save(LF_DF,file="LL_LF_5.RData")

# PS LF data
for (year in 121:256) {
  # year=210
  LF <- sim_i$obs[[paste0("simulated_lf_ps_",year)]]
  
  if(length(LF$data$obs)>40) LF_DF_year <- data.frame(LF$data$obs) %>% mutate(Year=year) # more than 1 obs
  if(length(LF$data$obs)==40) LF_DF_year <- data.frame(t(LF$data$obs)) %>% mutate(Year=year) # 1 obs
  
  if(year==121) LF_DF0 <- LF_DF_year
  else LF_DF0 <- rbind(LF_DF0,LF_DF_year)                                 
}

names(LF_DF0)[1] <- "LatLon"
names(LF_DF0)[2:40] <- LF$data$length_bins[1:39]

LF_DF0 <- unique(LF_DF0)

LF_DF <- LF_DF0 %>% gather(2:40,key="Length",value = "LF") %>%
  mutate(Length=as.numeric(Length),LF=as.numeric(LF)) %>%
  separate(LatLon, c("lat", "lon"), "-")

flag <- LF_DF %>% group_by(Year,lat,lon) %>% summarise(s=sum(LF))


LF_DF <- left_join(left_join(LF_DF,Lat_grid),Lon_grid) %>%
  mutate(Lat=as.numeric(levels(Lat))[Lat],Lon=as.numeric(levels(Lon))[Lon])

save(LF_DF, file="PS_LF_5.RData")

# PS catch data
for (year in 1:256) {
  # year=210
  Catch <- sim_i$layers[[paste0("layer[fishing_ps_",year,"]")]]

  Catch_DF_year <- data.frame(Catch = as.numeric(Catch$data[1:(13*17)]),
                              Lat = rep(Lat_grid$Lat,17),
                              Lon = rep(Lon_grid$Lon,each=13),
                              Year = year)

  if(year==1) Catch_DF <- Catch_DF_year
  else Catch_DF <- rbind(Catch_DF,Catch_DF_year)
}

save(Catch_DF, file="PS_Catch_5.RData")

# LL catch data
for (year in 1:256) {
  # year=210
  Catch <- sim_i$layers[[paste0("layer[fishing_ll_",year,"]")]]
  
  Catch_DF_year <- data.frame(Catch = as.numeric(Catch$data[1:(13*17)]),
                              Lat = rep(Lat_grid$Lat,17),
                              Lon = rep(Lon_grid$Lon,each=13),
                              Year = year)
  
  if(year==1) Catch_DF <- Catch_DF_year
  else Catch_DF <- rbind(Catch_DF,Catch_DF_year)
}

save(Catch_DF, file="LL_Catch_5.RData")

}

library(tidyverse)

load("Data/sim_1.RData")
# load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/25/sim_4.RData")

Lat_grid <- data.frame("lat" = paste0("r",as.character(seq(1,13))),
                       "Lat" = sim_1$layers$`layer[latitude]`$data[,1])
Lon_grid <- data.frame("lon" = paste0("c",as.character(seq(1,17))),
                       "Lon" = sim_1$layers$`layer[longitude]`$data[1,])

rm(sim_1)
# load("Data/sim_4_v2.RData")
load("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/YFT_221cell_observations_1-100_ESS_25.RData")

# LL CPUE data
for (year in 81:256) {
  CPUE <- sim_4$obs[[paste0("simulated_cpue_ll_jpn_",year)]]
  
  if(year==81) Data_Geostat0 <- data.frame("Year"=year,"LatLon"=CPUE$data$obs[,1],"Catch_KG"=CPUE$data$obs[,2])
  else Data_Geostat0 <- rbind(Data_Geostat0,
                              data.frame("Year"=year,"LatLon"=CPUE$data$obs[,1],"Catch_KG"=CPUE$data$obs[,2]))                                   
}

Data_Geostat <- Data_Geostat0 %>%
  mutate(Catch_KG=ifelse(as.numeric(levels(Catch_KG))[Catch_KG]<1e-6,0,as.numeric(levels(Catch_KG))[Catch_KG])) %>%
  separate(LatLon, c("lat", "lon"), "-")

Data_Geostat <- left_join(left_join(Data_Geostat,Lat_grid),Lon_grid) %>%
  mutate(Lat=as.numeric(levels(Lat))[Lat],Lon=as.numeric(levels(Lon))[Lon])

f0 <- ggplot(data=Data_Geostat) +
  geom_point(aes(x=Lon,y=Lat)) +
  facet_wrap(~Year) +
  theme_bw()

ggsave(f0,file="Data/CPUE_25.png", width = 24, height = 16)

save(Data_Geostat,file="Data/CPUE_25.RData")

# LL LF data
for (year in 9:238) {
  # year=210
  LF <- sim_4$obs[[paste0("simulated_lf_ll_",year)]]
  
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

# LF_DF <- unique(LF_DF0) %>% gather(2:40,key="Length",value = "LF") %>%
#   mutate(Length=as.numeric(Length),LF=as.numeric(LF)) %>%
#   separate(LatLon, c("lat", "lon"), "-")

LF_DF <- left_join(left_join(LF_DF,Lat_grid),Lon_grid) %>%
  mutate(Lat=as.numeric(levels(Lat))[Lat],Lon=as.numeric(levels(Lon))[Lon])

f1 <- ggplot(data=LF_DF) +
  geom_point(aes(x=Lon,y=Lat)) +
  # facet_wrap(~Year) +
  theme_bw(12)

ggsave(f1,file="Data/LL_LF_25.png", width = 10, height = 8)

save(LF_DF,file="Data/LL_LF_25.RData")

# nominal LL LF
Data <- LF_DF0 %>% gather(2:40,key="Length",value = "LF") %>%
  # mutate(spp=floor(Length/10)*10) %>%
  mutate(Length=as.numeric(Length)) %>%
  group_by(Year,Length) %>% summarise(LF=sum(as.numeric(LF))) %>%
  spread(Length,LF)

size <- LF_DF0 %>% group_by(Year) %>%
  summarise(n=length(unique(LatLon))*5)

Data <- left_join(Data,size)

LL_LF <- cbind(1,Data$Year,1,17,0,0,Data$n,Data[,2:40]) %>%
  data.frame()

write.csv(LL_LF,file="Data/LL_LF_Nominal_25.csv",row.names = FALSE)


# PS LF data
for (year in 121:256) {
  # year=210
  LF <- sim_4$obs[[paste0("simulated_lf_ps_",year)]]
  
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

f2 <- ggplot(data=LF_DF) +
  geom_point(aes(x=Lon,y=Lat)) +
  # facet_wrap(~Year) +
  theme_bw()

ggsave(f2, file="Data/PS_LF_25.png", width = 10, height = 8)

save(LF_DF, file="Data/PS_LF_25.RData")


# Troll LF data
for (year in 134:255) {
  # year=210
  LF <- sim_4$obs[[paste0("simulated_lf_trol_",year)]]

  if(length(LF$data$obs)>40) LF_DF_year <- data.frame(LF$data$obs) %>% mutate(Year=year) # more than 1 obs
  if(length(LF$data$obs)==40) LF_DF_year <- data.frame(t(LF$data$obs)) %>% mutate(Year=year) # 1 obs

  if(year==134) LF_DF0 <- LF_DF_year
  else LF_DF0 <- rbind(LF_DF0,LF_DF_year)
}

names(LF_DF0)[1] <- "LatLon"
names(LF_DF0)[2:40] <- LF$data$length_bins[1:39]

LF_DF <- LF_DF0 %>% gather(2:40,key="Length",value = "LF") %>%
  mutate(Length=as.numeric(Length),LF=as.numeric(LF)) %>%
  separate(LatLon, c("lat", "lon"), "-")

LF_DF <- left_join(left_join(LF_DF,Lat_grid),Lon_grid) %>%
  mutate(Lat=as.numeric(levels(Lat))[Lat],Lon=as.numeric(levels(Lon))[Lon])

f3 <- ggplot(data=LF_DF) +
  geom_point(aes(x=Lon,y=Lat)) +
  facet_wrap(~Year) +
  theme_bw()

ggsave(f3, file="Data/Troll_LF.png", width = 10, height = 8)

save(LF_DF, file="Data/Troll_LF.RData")


# PS catch data
for (year in 1:256) {
  # year=210
  Catch <- sim_4$layers[[paste0("layer[fishing_ps_",year,"]")]]
  
  Catch_DF_year <- data.frame(Catch = as.numeric(Catch$data[1:(13*17)]),
                              Lat = rep(Lat_grid$Lat,17),
                              Lon = rep(Lon_grid$Lon,each=13),
                              Year = year)
  
  if(year==1) Catch_DF <- Catch_DF_year
  else Catch_DF <- rbind(Catch_DF,Catch_DF_year)
}

save(Catch_DF, file="Data/PS_Catch_25.RData")


# LL catch data
for (year in 1:256) {
  # year=210
  Catch <- sim_4$layers[[paste0("layer[fishing_ll_",year,"]")]]
  
  Catch_DF_year <- data.frame(Catch = as.numeric(Catch$data[1:(13*17)]),
                              Lat = rep(Lat_grid$Lat,17),
                              Lon = rep(Lon_grid$Lon,each=13),
                              Year = year)
  
  if(year==1) Catch_DF <- Catch_DF_year
  else Catch_DF <- rbind(Catch_DF,Catch_DF_year)
}

save(Catch_DF, file="Data/LL_Catch_25.RData")
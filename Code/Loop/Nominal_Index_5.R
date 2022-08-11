library(VAST)
library(tidyverse)

for (i in 1:100) {
  
  load(paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",toString(i),"/CPUE_5.RData"))

  CPUE <- Data_Geostat %>%
    group_by(Year) %>%
    summarise(cpue=mean(Catch_KG))
  
  write.csv(CPUE,row.names = FALSE,
  file=paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/Loop/",i,"/Nominal_Index.csv"))
  
}
  
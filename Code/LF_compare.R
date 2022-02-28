# Compare Nominal and VAST LF

library(tidyverse)

Nominal <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/SpatialAssessModelling/Data/LL_LF_Nominal_25.csv")

Nominal <- Nominal[,c(2,9:46)]
names(Nominal) <- c("Year",seq(10,195,5))  

Nominal_long <- Nominal %>%
  gather(2:39,key="Length",value="lf") %>%
  mutate(Length=as.numeric(Length)) %>%
  mutate(Length=ifelse(Length>180,180,floor(Length/10)*10)) %>%
  filter(Year>80,Length>10) %>%
  group_by(Year,Length) %>%
  summarise(lf=sum(lf)) %>%
  group_by(Year) %>%
  mutate(LF=lf/sum(lf)) %>%
  data.frame()

Nominal_mean <- Nominal_long %>%
  group_by(Length) %>%
  summarise(LF_mean=mean(LF))

# ggplot(data=Nominal_long) +
#   geom_point(aes(x=Length,y=LF)) +
#   facet_wrap(~Year)

ggplot(data = Nominal_mean) +
  geom_line(aes(x=Length,y=LF_mean))


VAST <- read.csv("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/VAST_LF/VAST_LF_25/SS.csv")

VAST <- VAST[,c(2,8:24)]
names(VAST) <- c("Year",seq(20,180,10))  

VAST_long <- VAST %>%
  gather(2:18,key="Length",value="LF") %>%
  mutate(Length=as.numeric(Length)) %>%
  # filter(Year>80) %>%
  # group_by(Year) %>%
  # mutate(LF=lf/sum(lf)) %>%
  data.frame()

VAST_mean <- VAST_long %>%
  group_by(Length) %>%
  summarise(LF_mean=mean(LF))

ggplot() +
  geom_line(aes(x=Length,y=LF_mean),data = VAST_mean,color="red") +
  geom_line(aes(x=Length,y=LF_mean),data = Nominal_mean,color="blue") +
  theme_bw() +
  ggtitle("Red: VAST; Blue: Nominal")


# # 
Nominal_long$Model <- "Nominal"
VAST_long$Model <- "VAST"

All <- rbind(Nominal_long[,c(1,2,4,5)],VAST_long)

ggplot(data=All) +
  geom_line(aes(x=Length,y=LF,color=Model)) +
  facet_wrap(~Year) +
  theme_bw()

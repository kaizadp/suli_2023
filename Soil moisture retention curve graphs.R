# Jack Kagan
# Flooding Experiment hPa and %water graphs
# 7/13/2023


getwd()


Bcontrol<-read.csv("C:\Users\kaga648\Downloads\hPa_water%")

library(tidyverse)
ggplot(Bcontrol, aes(y=MPa, x=Water.Content..Vol.., log=x))+
  geom_line()


Bcontrol<-`B.Control.Water%.and.hPa`
Acontrol<-`Acontrol.hPa.and.water%`
OWB<-`OWB.hPa.and.water%`
OWA<-`OWA.hPa.and.%water`
RRA<-`RRA.hPa.and.%water`
RRB<-`RRB.hPa.and.%water`  

MPa_water <- ggplot() +
  geom_line(data = Acontrol, aes(x=MPa, y=Water.Content..Vol.., colour = "A control"))+
  geom_line(data = Bcontrol, aes(x=MPa, y=Water.Content..Vol.., colour = "B control"))+
  geom_line(data = OWB, aes(x=MPa, y=Water.Content..Vol.., colour = "OWB"))+
  geom_line(data = OWA, aes(x=MPa, y=Water.Content..Vol.., colour = "OWA"))+
  geom_line(data = RRA, aes(x=MPa, y=Water.Content..Vol.., colour = "RRA"))+
  geom_line(data = RRB, aes(x=MPa, y=Water.Content..Vol.., colour = "RRB"))+
  ylab("Volumetric Water Content (%) ")+
  ggtitle("Soil Moisture Retention Curves for First Round of Samples")+
  scale_colour_manual("",
                      breaks = c("A control", "B control", "OWB", "OWA", "RRA", "RRB"),
                      values = c("red", "green", "blue", "purple", "orange", "black"))

MPa_water_A <- ggplot()+
  geom_line(data = Acontrol, aes(x=log(MPa), y=Water.Content..Vol.., colour = "A control"))+
  geom_line(data = OWA, aes(x=log(MPa), y=Water.Content..Vol.., colour = "OWA"))+
  geom_line(data = RRA, aes(x=log(MPa), y=Water.Content..Vol.., colour = "RRA"))+
  ylab("Volumetric Water Content (%) ")+
  ggtitle("Soil Moisture Retention Curves for First Round of Samples A Horizon")+
  scale_colour_manual("",
                      breaks = c("A control", "OWA", "RRA"),
                      values = c("black", "blue", "red"))+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(legend.text = element_text(size=12))

MPa_water_B <- ggplot() +
  geom_line(data = Bcontrol, aes(x=log(MPa), y=Water.Content..Vol.., colour = "B control"))+
  geom_line(data = OWB, aes(x=log(MPa), y=Water.Content..Vol.., colour = "OWB"))+
  geom_line(data = RRB, aes(x=log(MPa), y=Water.Content..Vol.., colour = "RRB"))+
  ylab("Volumetric Water Content (%) ")+
  ggtitle("Soil Moisture Retention Curves for First Round of Samples B Horizon")+
  scale_colour_manual("",
                      breaks = c("B control", "OWB","RRB"),
                      values = c("black", "blue", "red"))+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(legend.text = element_text(size=12))+
  ylim(0, 125)







MPa_water2 <- ggplot() +
  geom_line(data = Acontrol, aes(x=MPa, y=Water.Content..Vol.., colour = "A control"))+
  geom_line(data = Bcontrol, aes(x=MPa, y=Water.Content..Vol.., colour = "B control"))+
  geom_line(data = OWB, aes(x=MPa, y=Water.Content..Vol.., colour = "OWB"))+
  geom_line(data = OWA, aes(x=MPa, y=Water.Content..Vol.., colour = "OWA"))+
  geom_line(data = RRA, aes(x=MPa, y=Water.Content..Vol.., colour = "RRA"))+
  geom_line(data = RRB, aes(x=MPa, y=Water.Content..Vol.., colour = "RRB"))+
  ylab("Volumetric Water Content (%) ")+
  ggtitle("Soil Moisture Retention Curves for First Round of Samples")+
  scale_x_reverse()+
  scale_colour_manual("",
                      breaks = c("A control", "B control", "OWB", "OWA", "RRA", "RRB"),
                      values = c("red", "green", "blue", "purple", "orange", "black"))

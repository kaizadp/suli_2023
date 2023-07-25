# Jack Kagan
# Flooding Experiment oxygen graphs
# 7/17/2023



library(tidyverse)

RRB1_Oxygen_hrs<-RRB1.Oxygen.with.hours
ggplot( data = RRB1_Oxygen_hrs, aes (x=Times.hrs., y=Oxygen..mg.L.))+
  geom_line()
RRA1_Oxygen_hrs<-RRB1.O
FWA1_Oxygen_hrs<-FWA1.Oxy
FWB1_Oxygen_hrs<-FWB1.O

Oxygen_Graph <- ggplot() +
  geom_line(data = RRB1_Oxygen_hrs, aes(x=Times.hrs., y=Oxygen..mg.L., colour = "RRB1"))+
  geom_line(data = RRA1_Oxygen_hrs, aes(x=Times.hrs., y=Oxygen..mg.L., colour = "RRA1"))+
  geom_line(data = FWA1_Oxygen_hrs, aes(x=Times.hrs., y=Oxygen..mg.L., colour = "FWA1"))+
  geom_line(data = FWB1_Oxygen_hrs, aes(x=Times.hrs., y=Oxygen..mg.L., colour = "FWB1"))+
  ylab("Oxygen Content (mg/L)")+
  xlab("Time (hours)")+
  xlim(0,97.1)+
  ggtitle("Oxygen Data for Second Round of Flooding")+
  scale_colour_manual("",
                      breaks = c("RRA1", "RRB1", "FWA1", "FWB1"),
                      values = c("red","blue", "purple","black"))+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(legend.text = element_text(size=12))

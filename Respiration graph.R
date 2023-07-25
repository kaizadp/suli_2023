# Jack Kagan
# Respiration Graphs
# 7/17/2023


library(tidyverse)
Respiration<-Respiration.2nd.Cycle

Respiration_new<- Respiration %>% 
  mutate(group = factor(group, levels = c("After 1st Drain", "After 2nd Flood","After 2nd Drain")))

ggplot(Respiration_new, aes (x=Sample, y=CO2.ppm, fill = Sample))+
  geom_bar(stat = 'identity')+
  facet_wrap(~group)+
  scale_fill_manual(values=c("black", "lightblue", "blue", "red", "darkred"))+
  ylab("CO2 ppm")+
  xlab("Sample ID")+
  ggtitle("Respiration Data for 2nd Round of Flooding")+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(legend.text = element_text(size=12))




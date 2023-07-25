# Surface area data






library(tidyverse)
Surface_area <- Surface.area.data


ggplot(Surface_area, aes (y=sample.depth, x=S, fill = sample.depth))+
  geom_bar(stat = 'identity')+
  facet_wrap(~core)+
  scale_fill_manual(values=c("#bfe6ff", "#8cd3ff", "#26abff", "#1167b1", "#03254c"),
                    name = "sample depth (cm)")+
  ylab("Specific Surface Area")+
  xlab("Sample Depth (cm)")+
  ggtitle("Specific Surface Area for B Horizon Samples")+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size = 12))+
  theme(legend.position = "none")

new_SSA<-Copy.of.specific.surface.area
OWB_SSA <- new_SSA %>% 
  filter(core == "OWB") 

RRB_SSA <- new_SSA %>%  
  filter(core == "RRB")

RRA_SSA <- new_SSA %>%  
  filter(core == "RRA")

OWA_SSA <- new_SSA %>%  
  filter(core == "OWA")

Acontrol_SSA <- new_SSA %>% 
  filter(core=="TempA")

Bcontrol_SSA <- new_SSA %>%  
  filter(core == "TempB")


SSA_graph <-  ggplot()+
  geom_point(data = OWB_SSA, aes(y=sample.depth, x=S, colour = "OWB"))+
  geom_point(data = RRB_SSA, aes(y=sample.depth, x=S, colour = "RRB"))+
  geom_point(data = OWA_SSA, aes(y=sample.depth, x=S, colour = "OWA"))+
  geom_point(data = RRA_SSA, aes(y=sample.depth, x=S, colour = "RRA"))+
  geom_point(data = Acontrol_SSA, aes(y=sample.depth, x=S, colour = "A Control"))+
  geom_point(data = Bcontrol_SSA, aes(y=sample.depth, x=S, colour = "B Control"))+
  geom_line(data = OWB_SSA, aes(y=sample.depth, x=S, colour = "OWB"), orientation = "y")+
  geom_line(data = RRB_SSA, aes(y=sample.depth, x=S, colour = "RRB"), orientation = "y")+
  geom_line(data = OWA_SSA, aes(y=sample.depth, x=S, colour = "OWA"), orientation = "y")+
  geom_line(data = RRA_SSA, aes(y=sample.depth, x=S, colour = "RRA"), orientation = "y")+
  geom_line(data = Acontrol_SSA, aes(y=sample.depth, x=S, colour = "A Control"), orientation = "y")+
  geom_line(data = Bcontrol_SSA, aes(y=sample.depth, x=S, colour = "B Control"), orientation = "y")+
  ylab("Sample Depth (cm)")+
  xlab("Specific Surface Area")+
  ggtitle("Soil Moisture Retention Curves for First Round of Samples A Horizon")+
  facet_wrap(~group)+
  scale_colour_manual("",
                      breaks = c("OWB", "RRB", "OWA", "RRA", "A Control", "B Control"),
                      values = c("blue", "red", "darkblue", "darkred", "black", "black"))+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(legend.text = element_text(size=12))+
  scale_y_reverse()
  


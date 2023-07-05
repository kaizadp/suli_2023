# Jack Kagan
# 6/28/23
# EC1 Data


library(tidyverse)
glimpse(ec1_soil_bulk_density_L2)
str(ec1_soil_bulk_density_L2)
summary(ec1_soil_bulk_density_L2)

glimpse(ec1_soil_tn_L2)
str(ec1_soil_tn_L2)
summary(ec1_soil_tn_L2)


# reordering so that it is upland, transition, wetland
ec1_n<-
  ec1_soil_tn_L2 %>% 
  mutate(transect_location = factor(transect_location, levels = c("upland", "transition",
                                                                 "wetland")))
# merging great lakes and mid-atlantic
ec1_merge<-merge(ec1_metadata_kitlevel, ec1_n)

# boxplot for great lakes
great_lakes_n_plot<-ec1_merge %>% 
  filter(region=="Great Lakes") %>% 
  ggplot(aes(x=transect_location, y=nitrogen_weight_perc, fill=transect_location))+
  geom_boxplot(width=0.6, alpha=0.7)+
  geom_jitter(position=position_jitter(0.15))+
  labs(title = "Great Lakes N Weight % Across Transects",
       x = "Transect Location",
       y ="N Weight %",)+
  theme(legend.position = "none")+
  scale_fill_manual(values =c("#a4d6cc", "#58a2b3", "#1f526e"))

# boxplot for mid-atlantic
mid_atlantic_n_plot<-ec1_merge %>% 
  filter(region=="Mid-Atlantic") %>% 
  ggplot(aes(x=transect_location, y=nitrogen_weight_perc, fill=transect_location))+
  geom_boxplot(width=0.6, alpha=0.7)+
  geom_jitter(position=position_jitter(0.15))+
  labs(title = "Mid-Atlantic N Weight % Across Transects",
       x = "Transect Location",
       y ="N Weight %")+
  theme(legend.position = "none")+
  scale_fill_manual(values =c("#a4d6cc", "#58a2b3", "#1f526e"))

# subset for great lakes
great_lakes_n<-ec1_merge %>% 
  filter(region=="Great Lakes")

# subset for mid-atlantic
mid_atlantic_n<-ec1_merge %>% 
  filter(region=="Mid-Atlantic")

# anova for mid-atlantic
anov_ma <- aov(nitrogen_weight_perc ~ transect_location, data = mid_atlantic_n)
summary(anov_ma)

anov_gl <- aov(nitrogen_weight_perc ~ transect_location, data = great_lakes_n)
summary(anov_gl)













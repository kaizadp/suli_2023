# imported data from files using Import Dataset

# soil carbon weight percent by transect, jitter plot:
ec1_soil_tc_L2 %>%
  ggplot(aes(x = transect_location, y = carbon_weight_perc))+ 
  geom_jitter(width=0.2, alpha = 0.3, color = "red")+
  labs(x = "Transect Location", y = "Carbon Weight (%)", title = "Percent of Carbon in Each Transect")+
  theme(plot.title = element_text(hjust = 0.5))

# reorder the transects to be in upland, transition, wetland order
soil_carbon2 <-
  ec1_soil_tc_L2 %>%
  mutate(transect_location = factor(transect_location, levels = c("upland", "transition", "wetland")))

# using the reordered transects, make boxplot of carbon weight percent by transect,
# filled by each transect (using set3 palette):
soil_carbon2 %>%
  ggplot(aes(x = transect_location, y = carbon_weight_perc, fill = transect_location))+ 
  geom_boxplot()+
  geom_jitter(width=0.2, alpha = 0.3)+
  labs(x = "Transect Location", y = "Carbon Weight (%)", title = "Percent of Carbon in Each Transect")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Set3")

#average carbon weight percent for each transect location:
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_percent_mean = mean(carbon_weight_perc), n = n())

#median carbon weight percent for each transect location:
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_percent_median = median(carbon_weight_perc), n = n())

#standard deviation of carbon weight percent for each transect location:
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_percent_sd = sd(carbon_weight_perc), n = n())

# imported data from files using Import Dataset>From Text(base)

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

# using the reordered transects, make boxplot (with jitter) of
# carbon weight percent by transect,
# filled by each transect (using set3 palette):
soil_carbon2 %>%
  ggplot(aes(x = transect_location, y = carbon_weight_perc, fill = transect_location))+ 
  geom_boxplot()+
  geom_jitter(width=0.2, alpha = 0.3)+
  labs(x = "Transect Location", y = "Carbon Weight (%)", title = "Percent of Carbon in Each Transect")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Set3")+
  guides(fill = guide_legend(title = "Transect Location"))

# average carbon weight percent for each transect location:
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_percent_mean = mean(carbon_weight_perc), n = n())

# median carbon weight percent for each transect location:
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_percent_median = median(carbon_weight_perc), n = n())

# standard deviation of carbon weight percent for each transect location:
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_percent_sd = sd(carbon_weight_perc), n = n())

# is there a way to run all three summaries at once?
soil_carbon2 %>%
  group_by(transect_location) %>%
  dplyr::summarise(carbon_perc_mean = mean(carbon_weight_perc), n = n()) %>%
  dplyr::summarise(carbon_perc_median = median(carbon_weight_perc), n = n()) %>%
  dplyr::summarise(carbon_perc_sd = sd(carbon_weight_perc), n = n())

# merge two datasets into one:
merged <- merge(ec1_metadata_kitlevel, soil_carbon2)

# from the merged datasets, create a boxplot showing carbon weight percent by region:
merged %>%
  ggplot(aes(x = region, y = carbon_weight_perc, fill = region))+ 
  geom_boxplot()+
  geom_jitter(width=0.2, alpha = 0.3)+
  labs(x = "Region", y = "Carbon Weight (%)", title = "Percent of Carbon in Each Region")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Accent")+
  guides(fill = guide_legend(title = "Region"))

# tried ANOVA:

a = aov(carbon_weight_perc ~ transect_location, data = merged)
summary(a)

b = aov(carbon_weight_perc ~ region, data = merged)
summary(b)

ab = aov(carbon_weight_perc ~ transect_location + region, data = merged)
summary(ab)

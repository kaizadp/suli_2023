#READ IN DATA ----
setwd("C:\\Users\\spen761\\Documents\\EXCHANGE Soil Data\\ec1_soil_v1")
bulkdensity <- read.csv("ec1_soil_bulk_density_L2.csv")
soilcond  <- read.csv("ec1_soil_cond_L2.csv")
soilgwc <- read.csv("ec1_soil_gwc_L2.csv")
soilph <- read.csv("ec1_soil_ph_L2.csv")
soiltc <- read.csv("ec1_soil_tc_L2.csv")
soiltn <- read.csv("ec1_soil_tn_L2.csv")
metadata <- read.csv("ec1_metadata_kitlevel.csv")
library(tidyverse)
#CODE ----

#reorder transect locations to upland, transition, wetland
soilph2 <- soilph %>% 
  mutate(transect_location = factor(transect_location, levels = c("upland", "transition", "wetland")))

#plot PH data as a boxplot for transect location and region
ph_plot <-
  ggplot(soilph2, aes(x = transect_location, y = ph, 
                         fill = transect_location)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(color = "black", size = 2,
              alpha = 0.5, width = 0.1)+
  scale_fill_manual(values = 
                      c("#41E975", "#2999AD", "#574BCD")) +
  xlab("Location") +
  ylab("pH")
ph_plot

ph_region <-
  ggplot(better_ph, aes(x = region, y = ph, fill = region))+
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(color = "black", size = 2,
              alpha = 0.5, width = 0.1)+
  scale_fill_manual(values = 
                      c("#41E975", "#2999AD")) +
  xlab("Region")+
  ylab("pH")
ph_region

#summarize pH
soilph %>% 
  group_by(transect_location) %>% 
  dplyr::summarise(pHmean = mean(ph),
                   n = n())
#merge meta data and soil ph
better_ph <- merge(metadata, soilph)


#TUKEY TEST pH
ph_mod <- lm(ph ~ transect_location, data = better_ph)
anova(ph_mod)
aov.ph <- aov(ph_mod)
TukeyHSD(aov.ph)

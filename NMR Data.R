library(tidyverse)
library(nmrrr)

data = nmrrr::nmr_import_spectra(path = "C:/Users/spen761/Downloads/adsorptive_fractionation", method = "mnova")
#A HORIZON STUFF
a_horizon = 
  data %>% 
  filter(ppm >= 0 & ppm <= 10) %>% 
  nmr_assign_bins(binset = bins_Clemente2012) %>% 
  filter(group != "oalkyl") %>% 
  filter(sampleID == "004-fit"| sampleID == "007-fit" | sampleID == "012-fit" | sampleID == "051-fit") %>% 
  mutate(intensity = if_else(intensity < 0, 0, intensity))

nmr_plot_spectra(a_horizon, 
                 binset = bins_Clemente2012,
                 label_position = 2.5,
                 stagger = 0.6,
                 mapping = aes(x = ppm, y = intensity, 
                               group = sampleID))+
  ylim(0, 3)

#B HORIZON STUFF
b_horizon = 
  data %>% 
  filter(ppm >= 0 & ppm <= 10) %>% 
  nmr_assign_bins(binset = bins_Clemente2012) %>% 
  filter(group != "oalkyl") %>% 
  filter(sampleID == "027-fit"| sampleID == "033-fit" | sampleID == "036-fit" | sampleID == "051-fit") %>% 
  mutate(intensity = if_else(intensity < 0, 0, intensity))

nmr_plot_spectra(b_horizon, 
                 binset = bins_Clemente2012,
                 label_position = 2.5,
                 stagger = 0.6,
                 mapping = aes(x = ppm, y = intensity, 
                               group = sampleID))+
  ylim(0, 3)

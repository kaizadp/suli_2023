library(tidyverse)
library(nmrrr)


data = nmrrr::nmr_import_spectra(path = "1-data/nmr/mnova_files", method = "mnova")
data2 = 
  data %>% 
  filter(ppm >= 0 & ppm <= 10) %>% 
  nmr_assign_bins(binset = bins_Clemente2012) %>% 
  filter(group != "oalkyl") %>% 
  mutate(intensity = if_else(intensity < 0, 0, intensity))

nmr_plot_spectra(data2, 
                 binset = bins_Clemente2012,
                 label_position = 3,
                 stagger = 0.5,
                 mapping = aes(x = ppm, y = intensity, 
                               group = sampleID))+
  ylim(0, 5)

relabund = nmr_relabund(data2, method = "AUC")

relabund %>% 
  ggplot(aes(x = sampleID, y = relabund, fill = group))+
  geom_bar(stat = "identity")

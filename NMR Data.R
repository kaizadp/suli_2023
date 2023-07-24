# load packages ----
library(tidyverse)
library(nmrrr)
library(gridExtra)

# 1. load and process data ----
# load sample key
sample_key = googlesheets4::read_sheet("1ix8ckXv4hwVZ6KBD4ke_BHO_8iBe87CFBCUb7YbS-9E")

# import nmr spectra data using `nmrrr` package
data = nmrrr::nmr_import_spectra(path = "data/adsorptive-fractionation/nmr", method = "mnova")

# process nmr data
data2 =
  data %>% 
  filter(ppm >= 0 & ppm <= 10) %>% 
  nmr_assign_bins(binset = bins_Clemente2012) %>% 
  # now, format the sampleID column to match the sample_key dataframe
  mutate(sampleID = str_remove(sampleID, "-fit"),
         sampleID = paste0("ads_", sampleID)) %>% 
  # now, join the sample key
  left_join(sample_key, by = c("sampleID" = "sample_name"))

## TO BE DELETED:
##  mutate(treatment = case_when(
##    startsWith(sampleID, "004") ~ "Control",
##    startsWith(sampleID, "007") ~ "Acid",
##    startsWith(sampleID, "010") ~ "Acid",
##    startsWith(sampleID, "012") ~ "Alkaline",
##    startsWith(sampleID, "015") ~ "Alkaline",
##    startsWith(sampleID, "027") ~ "Control",
##    startsWith(sampleID, "033") ~ "Acid",
##    startsWith(sampleID, "034") ~ "Acid",
##    startsWith(sampleID, "036") ~ "Alkaline",
##    startsWith(sampleID, "037") ~ "Alkaline")) %>% 
##  mutate(intensity = if_else(intensity < 0, 0, intensity)) 

#
# 2. plot spectra ----

#A HORIZON SPECTRA ----

#filter out the A horizon samples
a_horizon <-
  data2 %>% 
  filter(horizon == "A horizon")

#plot a horizon spectra with color determined by treatment
nmr_plot_spectra(a_horizon, 
                 binset = bins_Clemente2012,
                 label_position = 5.5,
                 stagger = 1,
                 mapping = aes(x = ppm, y = intensity, group = sampleID, color = treatment))+
  ylim(0, 6)+
  scale_color_manual(name = "Treatment", values = c("#ef476f", "#06d6a0", "#118ab2"))+
  xlab("Shift (ppm)")+
  ylab("Intensity")+
  ggtitle("A Horizon")+
  theme(plot.title = element_text(hjust = 0.5))


#B HORIZON SPECTRA ----
b_horizon <-
  data2 %>% 
  filter(horizon == "B horizon")

nmr_plot_spectra(b_horizon, 
                 binset = bins_Clemente2012,
                 label_position = 5.5,
                 stagger = 1,
                 mapping = aes(x = ppm, y = intensity, 
                               group = sampleID, color = treatment))+
  ylim(0, 6)+
  scale_color_manual(name = "Treatment", values = c("#ffd166", "#073b4c", "#118ab2"))+
  xlab("Shift (ppm)")+
  ylab("Intensity")+
  ggtitle("B Horizon")+
  theme(plot.title = element_text(hjust = 0.5))


# 
# 3. calculate relative abundance ----

# calculate relative abundance per sample
nmr_relabund = 
  data2 %>% 
  # remove oalkyl group
  filter(group != "oalkyl") %>% 
  nmr_relabund(method = "AUC") %>% 
  left_join(sample_key, by = c("sampleID" = "sample_name")) %>% 
  filter(horizon != "blank")

# plot
nmr_relabund %>% 
  ggplot(aes(x = sampleID, y = relabund, fill = group))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "Group", values = c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c"))+
  facet_wrap(~horizon + treatment, scales = "free_x")+
  xlab("Treatment")+
  ylab("Relative Abundance")+
  ggtitle("A Horizon")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45))

# load packages ----
library(tidyverse)
library(nmrrr)
library(gridExtra)

#load data ----
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
  mutate(sample_name = str_remove(sampleID, "-fit"),
         sample_name = paste0("ads_", sample_name)) %>% 
  # now, join the sample key
  left_join(sample_key)

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
#A HORIZON SPECTRA ----

#filter out the A horizon samples
a_horizon <-
  data2 %>% 
  filter(horizon == "A horizon")
  filter(sampleID == "004-fit"| sampleID == "007-fit" | sampleID == "010-fit" | sampleID == "012-fit" | sampleID == "015-fit")
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

#A ABUNDANCE ----

#filter out oalkyl group for a horizon samples
mod_a_horizon <-
  a_horizon %>% 
  filter(group != "oalkyl")
#get relative abundances
relabund_a = nmr_relabund(mod_a_horizon, method = "AUC")

#reassign treatments to the relative abundances
sum_a <-
  relabund_a %>% 
  mutate(treatment = case_when(
    startsWith(sampleID, "004") ~ "Control",
    startsWith(sampleID, "007") ~ "Acid",
    startsWith(sampleID, "010") ~ "Acid",
    startsWith(sampleID, "012") ~ "Alkaline",
    startsWith(sampleID, "015") ~ "Alkaline"))

#bar graph of relative abundances
rel_a <-
  sum_a %>% 
  ggplot(aes(x = treatment, y = relabund, fill = group))+
    geom_bar(stat = "identity")+
    scale_fill_manual(name = "Group", values = c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c"))+
    xlab("Treatment")+
    ylab("Relative Abundance")+
    ggtitle("A Horizon")+
    theme(plot.title = element_text(hjust = 0.5))

#B HORIZON SPECTRA ----
b_horizon <-
  data2 %>% 
  filter(sampleID == "027-fit"| sampleID == "033-fit" | sampleID == "034-fit" | sampleID == "036-fit" | sampleID == "037-fit")

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
#B ABUNDANCE ----

relabund_b = nmr_relabund(b_horizon, method = "AUC")

rel_b <-
relabund_b %>% 
  ggplot(aes(x = sampleID, y = relabund, fill = group))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "Group", values = c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c"))+
  xlab("Treatment")+
  ylab("Relative Abundance")+
  ggtitle("B Horizon")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(rel_a,rel_b)

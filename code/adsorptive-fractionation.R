
# load packages -----------------------------------------------------------

library(tidyverse)
library(googlesheets4)

#

# load data ---------------------------------------------------------------

# import Shimadzu file
doc_data = read_tsv("data/adsorptive-fractionation/2023-07-11_kfp_adsorptive_fractionation.txt", skip = 10)
doc_data_subset = 
  doc_data %>% 
  # clean up headers
  janitor::clean_names() %>% 
  dplyr::select(sample_name, result_npoc)

# import sample key
sample_key = read_sheet("1ix8ckXv4hwVZ6KBD4ke_BHO_8iBe87CFBCUb7YbS-9E")

# merge sample key to data file
doc_data_processed = 
  doc_data_subset %>% 
  left_join(sample_key)

doc_final =
  doc_data_processed %>% 
  filter(grepl("ads_", sample_name))

doc_initial =
  doc_data_processed %>% 
  filter(grepl("initial_", sample_name)) %>% 
  separate(sample_name, sep = "_", into = c("horizon", "initial", "treatment")) %>% 
  mutate(horizon = case_match(horizon, 
                              "ahorizon" ~ "A horizon", 
                              "bhorizon" ~ "B horizon")) %>% 
  rename(npoc_initial_mgl = result_npoc) %>% 
  dplyr::select(-initial)

doc_data_initial_final = 
  doc_final %>% 
  rename(npoc_final_mgl = result_npoc) %>% 
  left_join(doc_initial, by = c("horizon", "treatment")) %>% 
  mutate(doc_adsorbed_mgl = npoc_initial_mgl - npoc_final_mgl,
         doc_adsorbed_mgl = if_else(doc_adsorbed_mgl < 0, 0, doc_adsorbed_mgl)) %>% 
  filter(!is.na(doc_adsorbed_mgl))

# plot
doc_data_initial_final %>% 
  ggplot(aes(x = treatment, y = doc_adsorbed_mgl))+
  geom_point()+
  facet_wrap(~horizon)



## ANOVA
## aov()

aov_a = aov(doc_adsorbed_mgl ~ treatment, 
            data = doc_data_initial_final %>%  filter(horizon == "A horizon"))
summary(aov_a)

aov_b = aov(doc_adsorbed_mgl ~ treatment, 
            data = doc_data_initial_final %>%  filter(horizon == "B horizon"))
summary(aov_b)

# pair-wise comparisons
library(agricolae)

hsd_a = HSD.test(aov_a, "treatment")
hsd_b = HSD.test(aov_b, "treatment")

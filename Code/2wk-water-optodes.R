library(tidyverse)

#

# load data ---------------------------------------------------------------
optodes_data = read.csv("data/anoxia-redox/optode_processed_water_2wk_first24hr.csv")

optode_processed_water_2wk_first24hr %>% 
  ggplot(aes(x = time_minutes/60, y = do_mg_L))+
  geom_line(size = 0.5)+
  facet_wrap(~location)+
  
  labs(x = "Elapsed time, Hours", y = "Dissolved oxygen, mg/L")

transition_a <-
  optode_processed_water_2wk_first24hr %>%
  subset(location == "transition-A")

# how to smooth the lines by using group_by?
transition_a %>%
  ggplot(aes(x = time_minutes/60, y = do_mg_L, group_by(sample_label)))+
  geom_line()+
  xlim(0,2)+
  labs(x = "Elapsed time, Hours", y = "Dissolved oxygen, mg/L", title = "Transition")

upland_a <-
  optode_processed_water_2wk_first24hr %>%
  subset(location == "upland-A")

upland_a %>%
  ggplot(aes(x = time_minutes/60, y = do_mg_L))+
  geom_line()+
  labs(x = "Elapsed time, Hours", y = "Dissolved oxygen, mg/L", title = "Upland-A")

upland_b <-
  optode_processed_water_2wk_first24hr %>%
  subset(location == "upland-B")

upland_b %>%
  ggplot(aes(x = time_minutes/60, y = do_mg_L))+
  geom_line()+
  labs(x = "Elapsed time, Hours", y = "Dissolved oxygen, mg/L", title = "Upland-B")

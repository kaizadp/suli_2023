library(tidyverse)

#

# load data ---------------------------------------------------------------
optodes_data = read.csv("data/anoxia-redox/optode_processed_water_2wk_first24hr.csv")

optode_processed_water_2wk_first24hr %>% 
  ggplot(aes(x = time_minutes/60, y = do_mg_L))+
  geom_point()+
  facet_wrap(~location)+
  xlim(0, 2)+
  labs(x = "Elapsed time, Hours", y = "Dissolved oxygen, mg/L")

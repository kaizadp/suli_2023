
# load packages -----------------------------------------------------------

library(tidyverse)

#

# load data ---------------------------------------------------------------
optodes_data = read.csv("data/anoxia-redox/optode_processed_water_2wk_first24hr.csv")

optodes_data %>% 
  ggplot(aes(x = time_minutes/60, y = do_mg_L))+
  geom_point()+
  facet_wrap(~location)+
  xlim(0, 2)

# load packages ----

library(tidyverse)

# load dataset ----

villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# villagers data ----

# create dataset of each personality type and their count
villagers_summary <- 
  villagers %>% 
  group_by(personality) %>%
  summarize(p_counts = n())

# create graph of personality type by personality count
villagers_summary %>%
  ggplot(aes(x = personality, y = p_counts))+ geom_bar(stat="identity", fill="#FE4A49", width=.4)+
  theme(axis.text.x = element_text(size = 8))+ theme(plot.title = element_text(hjust = 0.5))+ labs(x = "Personality Type", y = "Personality Count", title = "Count of Each Personality Type in ACNH")


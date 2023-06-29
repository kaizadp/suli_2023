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

# create dataset of just chicken villagers, grouped by their name and personality
chicken_summary <-
  villagers %>%
  subset(species == "chicken") %>%
  group_by(personality, name) %>%
  summarize(chicken_count = n())

# graph chicken's name and their corresponding personality
chicken_summary %>%
  ggplot(aes(x = personality, y = name))+ geom_point()+
  theme(axis.text.x = element_text(size = 8))+ labs(x = "Personality of Chicken Villager", y = "Name of Chicken Villager", title = "Chicken Villagers' Names and Personalities in ACNH")

# trying to make personalities filled by gender or something
species_personality <-
  villagers %>%
  subset(species == "cat" | species == "dog") %>%
  group_by(personality, species, gender) %>%
  summarize(s_counts = n())

ggplot(species_personality, aes(x = personality, y = s_counts, fill = gender))+ 
  geom_bar(stat = "identity") + facet_wrap(~species)

             
# load packages ----

library(tidyverse)

# load dataset ----

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

# animal_rescues data, rescue of cats ----

rescue_summary <-
  animal_rescues %>%
  mutate(animal_group_parent = str_replace(animal_group_parent, "cat", "Cat")) %>%
  group_by(special_service_type_category, animal_group_parent) %>%
  summarize(incident_counts = n())

cat_rescue <-
  rescue_summary %>%
  subset(animal_group_parent == "Cat")

cat_rescue %>%
  ggplot(aes(x = special_service_type_category, y = incident_counts))+ geom_bar(stat="identity", fill="#17EF9A", width=.4)+
  theme(axis.text.x = element_text(angle = 0, size = 8, vjust = 0.6))+ labs(x = "Cat Rescue Service Category", y = "Rescue Counts")

# animal_rescues data ----

rescue <-
  animal_rescues %>%
  mutate(animal_group_parent = str_replace(animal_group_parent, "fox", "Fox")) %>%
  group_by(special_service_type_category, animal_group_parent) %>%
  summarize(incident_count = n())

fox_rescue <-
  rescue %>%
  subset(animal_group_parent == "Fox")

fox_rescue %>%
  ggplot(aes(x = special_service_type_category, y = incident_count))+ geom_bar(stat="identity", fill="#17EF9A", width=.4)+
  theme(axis.text.x = element_text(angle = 0, size = 8, vjust = 0.6))+ labs(x = "Fox Rescue Service Category", y = "Rescue Counts")

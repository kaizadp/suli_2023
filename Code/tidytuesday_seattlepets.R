# load packages -----------------------------------------------------------

library(tidyverse)

# seattle pets data: breed count by species -------------------------------------------------------

seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

pets_summary <- 
  seattle_pets %>% 
  mutate(primary_breed = str_replace(primary_breed, "-", " ")) %>% 
  group_by(species, primary_breed) %>% 
  summarize(breed_counts = n())
  #mutate(primary_breed = str_remove(primary_breed, "-")) %>% 

pets_summary %>% 
  ggplot(aes(x = primary_breed, y = breed_counts))+
  geom_point()+
  facet_wrap(~species, scales = "free_x")

pets_summary_greater_500 <- 
  pets_summary %>% 
  filter(breed_counts > 500)

pets_summary_greater_500 %>% 
  ggplot(aes(x = primary_breed, y = breed_counts))+
  geom_point()+
  facet_wrap(~species, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 45))

# seattle pets data: name count by species, subset cat ----

animal_summary <-
  cats <- subset(seattle_pets, species == "Cat")
  seattle_pets %>%
  group_by(species, animals_name) %>%
  summarize(name_counts = n())

animal_summary_greater_50 <-
  cats %>%
  filter(name_counts > 50)
# keeps saying name_counts is not found

animal_summary_greater_90 %>%
  ggplot(aes(x = animals_name, y = name_counts))+ geom_point()+
  facet_wrap(~species, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 45, size = 5))
  
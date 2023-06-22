# load in datasets ----

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
day_parts_map <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')

# load in packages ----

library(tidyverse)

# ufo_sightings data ----

# remove NA values
ufo_summary <-
  ufo_sightings %>%
  na.omit

# create column with the count of each ufo shape
shape_count <-
  ufo_summary %>%
  group_by(shape) %>%
  summarize(shape_counts = n())

# graph of each shape and their corresponding counts
shape_count %>%
  ggplot(aes(x = shape, y = shape_counts))+ geom_bar(stat="identity", fill="#17EF9A", width=.4)+
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.7))+ labs(x = "Observed Shape of UFO", y = "Shape Counts")

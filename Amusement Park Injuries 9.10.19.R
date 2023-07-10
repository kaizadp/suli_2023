# read in data / load packages ----
tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")
safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")
library(tidyverse)
library(extrafontdb)
library(extrafont)
font_import()
loadfonts(device = "win")
# code ----
summary(safer_parks)

glimpse(tx_injuries)
glimpse(safer_parks)

#GENDER INJURIES----
#Make all the gender letters uppercase
safer_parks$gender_upper = toupper(safer_parks$gender)
#make subset where injuries are equal to 1
single_injuries <- subset(safer_parks, num_injured == 1)
#new frame that shows frequency of injuries based on gender
gender_injuries <- data.frame(table(single_injuries$gender_upper))
#barplot shows injuries based on gender
genderplot <-
  gender_injuries %>%
  ggplot(aes(x = Var1 , y = Freq, fill = Var1)) +
  geom_bar(stat="identity", alpha=.9, width=.4) +
  scale_fill_manual(values = c("#F76960", "#6D99ED")) +
  xlab ("Gender") +
  ylab ("Number of Injuries")
genderplot

#RIDE INJURIES ----
#want to make a plot that shows injuries based on ride type

ride_type <-
  safer_parks %>% 
  group_by(device_category) %>% 
  count(device_category)

#smaller subset of rides that injuries were greater than 300
small_ride_type<- 
  ride_type %>% 
  filter(n>300) %>% 
  summarise(ridetype_injuries = sum(n)) %>% 
  mutate(device_category = fct_reorder(device_category, desc(ridetype_injuries)))

#bar graph showing injuries by ride greater than 300
rideplot <-
  small_ride_type %>% 
  ggplot(aes(x = device_category, y = ridetype_injuries, fill = device_category)) +
  geom_bar(stat = "identity", alpha=.9, width=.4) +
  labs(title = "Top 10 Most Dangerous Rides", 
       x = "Ride Type", 
       y = "Number of Injuries")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(
    text=element_text(size=16,  family="Rockwell"),
    legend.position = "none",
    plot.title = element_text(size = 25,hjust = 0.5, family = "Rockwell Extra Bold"))+
    scale_fill_brewer(palette = "Spectral", direction = 1)
rideplot


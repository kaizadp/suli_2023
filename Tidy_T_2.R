# Jack Kagan
# 6-20-2023
# Tidy Tuesday Pt 2

# Getting started with data set ----
ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')

library(plotly)
library(tidyverse)

# Taking a look at the dataset
str(ufo_sightings)
summary(ufo_sightings)



# Top countries subset ----

# Creating subset of top 2-9 countries for ufo sightings
# Excluded USA bc its proportion was so large
top_ufo <- ufo_sightings %>% 
  group_by(country_code)%>%
  count() %>% 
  filter(89 < n , n < 5000) %>%
  summarise(total_ufo = sum(n)) %>%
  mutate(total_ufo2 = sum(total_ufo))%>%
  mutate(prop_ufo = (total_ufo/total_ufo2)) %>% 
  mutate(country_code = fct_reorder(country_code,desc(prop_ufo)))

# Created bar plot from previous subset
top_ufo %>% 
  ggplot(aes(x = country_code, y = prop_ufo, fill = country_code))+
         geom_bar(stat = "identity", width = 0.5)+
  xlab("Country Code")+
  ylab("Prop of UFO Sightings")+
  theme(legend.position = "none")

# created new subset of top 5 countries for ufo sightings
# depicts how US dwarfs other countries, interesting
top_ufo2 <- ufo_sightings %>% 
  group_by(country_code) %>%
  count() %>%
  summarise(country_ufo = sum(n)) %>%
  mutate(total_ufo = sum(country_ufo)) %>%
  mutate(prop_ufo = (country_ufo / total_ufo)) %>% 
  filter(200 < country_ufo) %>%
  mutate(country_code = fct_reorder(country_code, desc(prop_ufo)))

# Created bar plot for previous subset
top_ufo2 %>% 
  ggplot(aes(x = country_code, y = prop_ufo, fill = country_code))+
  geom_bar(stat = "identity", width = 0.5)+
  xlab("Country Code")+
  ylab("Prop of UFO Sightings")+
  theme(legend.position = "none")



# Top cities subset ----
# subset of top 10 cities with ufo sightings
 city_ufo2 <- ufo_sightings %>% 
   group_by(city) %>% 
   count() %>% 
   summarise(city_total = sum(n)) %>% 
   arrange(desc(city_total)) %>%
   filter(city_total > 347) %>% 
   mutate(city = fct_reorder(city,desc(city_total)))

# created bar plot from previous subset
 city_ufo2 %>% 
   ggplot(aes(x = city, y = city_total, fill = city))+
   geom_bar(stat = "identity", width=.5) +
   labs(title = "UFO Sightings for Top 10 Cities",
        x = "City",
        y ="Number of UFO Sightings",
        fill ="Legend") + 
   scale_x_discrete(guide = guide_axis(angle = 45))+
   theme(legend.position = "none")

 


 
 
# Other stuff -----
 # Other code used in process
 # Not relevant but wanted to keep
 
 city_ufo <- ufo_sightings %>% 
   group_by(city) %>% 
   count() %>% 
   summarise(city_total=sum(n)) %>% 
   arrange(desc(city_total)) %>% 
   mutate(sum_city=sum(city_total)) %>% 
   mutate(prop_city=(city_total/sum_city)) %>% 
   filter(city_total>347) %>% 
   mutate(city=fct_reorder(city,desc(prop_city)))
 
 
 city_ufo %>% 
   ggplot(aes(x = city, y = prop_city, fill=city))+
   geom_bar(stat = "identity")+
   xlab("City")+
   ylab("Prop of UFO Sightings")
 
 # looking at top cities with ufo sightings
 # all US which is interesting
 ufo_sightings %>% 
   group_by(city) %>% 
   count() %>% 
   arrange(desc(n))
 
 


 
 
 
 
 
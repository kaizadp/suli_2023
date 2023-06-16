# load packages ----

# install.packages("tidytuesdayR")
# install.packages("forcats")
# install.packages("dplyr")

library(ggplot2)
library(forcats)
library(dplyr)

# load data ----
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

#plot that shows age based on birth date
plot(age ~ birth_date, centenarians)

#creates a new table of the frequency of different countries
country_counts<-data.frame(table(centenarians$place_of_death_or_residence))
country_counts

#subset of my country_counts table where the number of people in a country is greater than 1
small_country_counts <- subset(country_counts, Freq > 1)
small_country_counts

#sort alphabetically by country
sorted <- centenarians[order(centenarians$place_of_death_or_residence),]

## KP: tidyverse alternative
## sorted <- 
##   centenarians %>% 
##   arrange(place_of_death_or_residence) 
  
  

#bar chart showing count of people by country
ggplot(centenarians, aes(x = place_of_death_or_residence)) +
  geom_bar()


#bar chart showing people by country in cases where there is more than 1
countryplot <- 
  small_country_counts %>%
  mutate(Var1 = fct_reorder(Var1, desc(Freq))) %>%
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#6D99ED", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Country") +
  ylab("Number of Centenarians") +
  theme_bw()
countryplot

#different bar chart showing people by country in cases where there is more than 1
barplot(small_country_counts$Freq, names.arg = small_country_counts$Var1,
        xlab = "Country",ylab = "Count" ,main = "Most Centenarians Based on Country",
        col = "blue", las = 2)

############################################################
#basic stats
mean_age <- mean(centenarians$age)
mean_age
summary(centenarians$age)


#copied from school boxplot
#Boxplot shows average age for females and males
genderplot <- 
  ggplot(centenarians, aes(x = gender, y = age,
                           fill = gender)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(color = "black", size = 2,
              alpha = 0.5, width = 0.1) +
  scale_fill_manual(values = 
                      c("#F76960", "#6D99ED")) +
  xlab("Gender") +
  ylab("Age") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(face = "plain", color = "black", size = 14),
        axis.text.x = element_text(face = "italic", color = "black", size = 14),
        legend.text = element_text(face = "plain", color = "black", size = 12),
        legend.title = element_text(face = "bold", color = "black", size = 12),
        strip.text.x = element_text(face = "bold", color = "black", size = 18),
        strip.text.y = element_text(face = "bold", color = "black", size = 18),
        legend.key.size = unit(1.0, "cm"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.20, "cm"),
        axis.line.y = element_line(size = 0.5),
        axis.line.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border=element_rect(color = "black", size = 1, fill = NA))

#Way more concise code for boxplot
genderplot2.0 <- 
  centenarians %>%
  mutate(class = fct_reorder(gender, age, .fun='median')) %>%
  ggplot( aes(x= reorder(gender, age), y=age, fill=gender)) + 
  scale_fill_manual(values = c("#F76960","#6D99ED")) +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Age") +
  theme(legend.position="none")

#look at whether or not people born in the 1800s or 1900s have lived longer on average


#################
 #FINAL GRAPHS#
#################
countryplot
genderplot
genderplot2.0
################
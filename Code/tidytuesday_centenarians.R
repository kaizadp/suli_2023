# load packages -----------------------------------------------------------

library(ggplot2)


# centenarian data -------------------------------------------------------------
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')
ggplot(data=centenarians, mapping = aes(x = birth_date, y = age)) + geom_point(aes(color = gender)) + theme_bw()

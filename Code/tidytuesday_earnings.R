

# load packages -----------------------------------------------------------

library(ggplot2)


# earn data ---------------------------------------------------------------
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')
ggplot(data = earn, mapping = aes(x = sex, y = median_weekly_earn)) + geom_boxplot(aes(color = age)) + theme_bw()
ggplot(data = earn, mapping = aes(x = sex, y = median_weekly_earn)) + geom_boxplot(aes(color = race))



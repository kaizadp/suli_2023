
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

library(ggplot2)
ggplot(data = earn, mapping = aes(x = sex, y = median_weekly_earn)) + geom_boxplot(aes(color = age)) + theme_bw()
ggplot(data = earn, mapping = aes(x = sex, y = median_weekly_earn)) + geom_boxplot(aes(color = race))


library(ggplot2)
ggplot(data=centenarians, mapping = aes(x = birth_date, y = age)) + geom_point(aes(color = gender)) + theme_bw()

library(ggplot2)
ggplot(data=centenarians, mapping = aes(x = birth_date, y = age)) + geom_point(aes(color = gender)) + theme_bw()

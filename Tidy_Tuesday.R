# 5/30 week

# Either ISO-8601 date or year/week works!
install.packages("tidytuesdayR")
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')
str(centenarians)
centenarians$place_of_death_or_residence

install.packages("tidyverse")
install.packages("plotly")
library(plotly)
library(ggplot2)
library(tidytuesdayR)
age_rank_gender<-plot_ly(data=centenarians, x=~age
                         , y=~rank, color=~gender)%>%
  add_lines()
summary(centenarians)

age_rank_gender2<-plot_ly(data=centenarians, y=~age
                          , x=~rank, color=~gender)%>%
  add_lines()


# 6/6 week

owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')
summary(owid_energy)
str(owid_energy)
plot_ly(owid_energy, x=~oil_consumption,y=~country)
top_oil<-(owid_energy$oil_consumption>1000000)
plot_ly(owid_energy, x=~oil_consumption, y=~country)
top_oil

toptop_oil<-subset(owid_energy, oil_consumption>1000000, select = oil_consumption)

nuc<-(owid_energy$nuclear_share_energy>0)

nuclear<-subset(owid_energy, nuclear_share_energy>0)
str(nuclear)
plot_ly(nuclear, x=~country, y=~nuclear_share_energy)

plot_ly(owid_energy, x=~country, y=~coal_production)
coal<-subset(owid_energy, coal_production>0)
plot_ly(coal, x=~country, y=~coal_production)

summary(owid_energy)
USA<-subset(owid_energy, country=="United States")

oilc<-plot_ly(USA, x=~year, y=~oil_consumption)%>%
  layout(title="USA Oil Consumption")
coalp<-plot_ly(USA, x=~year, y=~coal_production)%>%
  layout(title="USA Coal Production")
plot_ly(USA, x=~year, y=~oil_production)
plot_ly(USA, x=~year, y=~renewables_share_energy)
str(USA)

subplot(oilc, coalp)%>%
  layout(title=list(text="Oil Consumption and Coal Production"))


USA_Russia<-subset(owid_energy, country=="United States" | country=="Russia")

USA_Russia_Oil_Prod<-plot_ly(USA_Russia, x=~year, y=~oil_production,
                             color=~country)%>%
  add_lines()

USA_Russia_Oil_Consump<-plot_ly(USA_Russia, x=~year, y=~oil_consumption,
                                color=~country)%>%
  add_lines()%>%
  layout(title="USA and Russia Oil Consumption")


USA_Russia_Coal<-plot_ly(USA_Russia, x=~year, y=~coal_consumption,
                         color=~country)%>%
  add_lines()%>%
  layout(title="USA and Russia Coal Consumption")

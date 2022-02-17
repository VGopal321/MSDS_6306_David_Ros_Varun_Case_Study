#Libraries
library(tidyverse)
library(caret)
library(class)
library(e1071)

#Import datasets
beers=read.csv(file.choose(),header=TRUE)
breweries=read.csv(file.choose(),header=TRUE)

#Filter out NAs in beers
beersclean=beers%>%filter(!(is.na(IBU)|is.na(ABV)))

#Find how many breweries there are per state
breweries%>%
  group_by(State)%>%
  summarize(count=n())%>%
  ggplot(aes(x=State,y=count))+
  geom_bar(stat='identity')
#commit update
  


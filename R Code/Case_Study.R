#Libraries
library(tidyverse)
library(caret)
library(class)
library(e1071)
library(maps)
library(mapproj)

#Import datasets
beers=read.csv(file.choose(),header=TRUE)
breweries=read.csv(file.choose(),header=TRUE)

#Filter out NAs in beers
beersclean=beers%>%filter(!(is.na(IBU)|is.na(ABV)))

#Find how many breweries there are per state

#Barplot of Breweries in Each State
breweries%>%
  group_by(State)%>%
  summarize(count=n())%>% 
  ggplot(aes(x=State,y=count))+
  geom_bar(stat='identity')

#Create Heat Map 
FiftyStates=data.frame(State=state.abb,Name=state.name) #Create and Name columns of DF
brewstates=breweries%>%group_by(State)%>%
  summarize(count=n())
brewstates=data.frame(brewstates)
for (i in 1:dim(brewstates)[1]){#Make sure the string values of FiftyStates and brewstates match for the upcoming merge
  brewstates$State[i]=str_extract(brewstates$State[i],"\\b[A-Za-z]+\\b")
}
brewstates=merge(brewstates,FiftyStates,'State',all.x=TRUE)
brewmapdata=data.frame(region=tolower(brewstates$Name),Breweries=brewstates$count)#Create mapdata with regions column

States=map_data('state')
map.df=merge(States,brewmapdata,"region",all.x=T)#Finalize Map Data
map.df=map.df[order(map.df$order),]

map.df%>%ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Breweries))+
  geom_path()+
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value = 'grey90')+
  ggtitle('Breweries by State')#Plot Heat Map


#Merge beer and breweries
beersclean$Brew_ID=beersclean$Brewery_id
beerbreweries=merge(beersclean,breweries,by="Brew_ID",all.x = TRUE)
beerbreweries=beerbreweries%>%select(!Brewery_id)
dim(beersclean)
dim(beerbreweries)

#Commit testing testing 123

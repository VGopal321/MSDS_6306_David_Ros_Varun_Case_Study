#Libraries
library(tidyverse)
library(caret)
library(class)
library(e1071)
library(maps)
library(mapproj)
library(plotly)

#Import datasets
beers=read.csv(file.choose(),header=TRUE)
breweries=read.csv(file.choose(),header=TRUE)

#Filter out NAs in beers
beersclean=beers%>%filter(!(is.na(IBU)|is.na(ABV)))

#Find how many breweries there are per state
# Grouped Breweries by volume
breweriesclean = breweries%>%group_by(State)%>%
  summarize(count=n())

breweriessummary = breweriesclean%>% 
  mutate(Group = case_when(
    between (count,1,2)~"1 to 2 Breweries",
    count ==3 ~"3 Breweries",
    count ==4 ~"4 Breweries",
    between (count,5,6)~"5 to 6 Breweries",
    between (count,7,9)~"7 to 9 Breweries",
    between (count,10,19)~"10 to 19 Breweries",
    between (count,20,29)~"20 to 29 Breweries",
    between (count,30,50)~"30+ Breweries"
  ))

#Barplot of Breweries in Each State.  Note: x=reorder, "-" before count orders decending, no "-" orders ascending

breweriessummary%>%
  ggplot(aes(x=reorder(State, count),y=count, fill=Group))+
  geom_bar(stat='identity', color = "grey46")+
  geom_text(aes(label  = count), vjust = -0.5, size = 2.5, color = "black")+
  ggtitle('Number of Breweries per State')+
  xlab('State')+
  ylab('Number of Breweries')+
  scale_fill_manual("Group",values = c("1 to 2 Breweries" = "white"
                                       ,"3 Breweries" = "seashell3"
                                       ,"4 Breweries" = "lightsteelblue"
                                       ,"5 to 6 Breweries"  = "lightskyblue1"
                                       ,"7 to 9 Breweries" = "deepskyblue"
                                       ,"10 to 19 Breweries" = "dodgerblue2"
                                       ,"20 to 29 Breweries" = "blue3"
                                       ,"30+ Breweries" = "midnightblue") )+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_blank(),
        title = element_text(face="bold", color = "red3", size = 8),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))+
  scale_y_continuous(minor_breaks = seq(0,50,10),breaks=seq(0,50,10))



#Create Heat Map 
FiftyStates=data.frame(State=state.abb,Name=state.name) #Create and Name columns of DF
brewstates=breweries%>%group_by(State)%>%
  summarize(count=n())
brewstates=data.frame(brewstates)
for (i in 1:dim(brewstates)[1]){#Make sure the string values of FiftyStates and brewstates match for the upcoming merge
  brewstates$State[i]=str_extract(brewstates$State[i],"\\b[A-Za-z]+\\b")
}
brewstates=merge(brewstates,FiftyStates,'State',all.x=TRUE)
brewmapdata=data.frame(region=tolower(brewstates$Name),Breweries=brewstates$count, State=brewstates$State)#Create mapdata with regions column

States=map_data('state')
map.df=merge(States,brewmapdata,"region",all.x=T)#Finalize Map Data
map.df=map.df[order(map.df$order),]

map.df%>%ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Breweries))+
  geom_path()+
  theme_void()+
  scale_fill_gradient2(low = "white", mid = "steelblue1", high = "midnightblue",midpoint = 25, limits = c(0,50), 
                       breaks=c(0,10,20,30,40,50), na.value = "grey50")+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(face="bold", color = "black", size = 8),
        title = element_text(face="bold", color = "red3", size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  ggtitle('Breweries by State')#Plot Heat Map
#Packages
install.packages("data.table")
install.packages("formattable")
install.packages("tidyr")

#Libraries
library(tidyverse)
library(caret)
library(class)
library(e1071)
library(maps)
library(mapproj)
library(plotly)
library(data.table)
library(formattable)
library(tidyr)
library(dplyr)



#Import datasets
beers=read.csv(file.choose(),header=TRUE)
breweries=read.csv(file.choose(),header=TRUE)

##QUESTION 3

#Filter out NAs in beers
beersclean=beers%>%filter(!(is.na(IBU)|is.na(ABV)))

##QUESTION 1

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
  scale_fill_manual("Group",values = c("1 to 2 Breweries" = "gray96"
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
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face="bold", color = "red3", size = 8),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.text.x = element_text(size = 7))+
  scale_y_continuous(minor_breaks = seq(0,50,10),breaks=seq(0,50,10))



#Create Heat Map 
FiftyStates=data.frame(State=state.abb,Name=state.name) #Create and Name columns of DF
brewstates=breweries%>%group_by(State)%>%
  summarize(count=n())
brewstates=data.frame(breweriessummary)
for (i in 1:dim(brewstates)[1]){#Make sure the string values of FiftyStates and brewstates match for the upcoming merge
  brewstates$State[i]=str_extract(brewstates$State[i],"\\b[A-Za-z]+\\b")
}
brewstates=merge(brewstates,FiftyStates,'State',all.x=TRUE)
brewmapdata=data.frame(region=tolower(brewstates$Name),Breweries=brewstates$count, State=brewstates$State, Group=brewstates$Group)#Create mapdata with regions column

States=map_data('state')
map.df=merge(States,brewmapdata,"region",all.x=T)#Finalize Map Data
map.df=map.df[order(map.df$order),]

map.df%>%ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Group))+
  geom_path(color = "grey46")+
  theme_void()+
  scale_fill_manual("Group",values = c("1 to 2 Breweries" = "gray96"
                                       ,"3 Breweries" = "seashell3"
                                       ,"4 Breweries" = "lightsteelblue"
                                       ,"5 to 6 Breweries"  = "lightskyblue1"
                                       ,"7 to 9 Breweries" = "deepskyblue"
                                       ,"10 to 19 Breweries" = "dodgerblue2"
                                       ,"20 to 29 Breweries" = "blue3"
                                       ,"30+ Breweries" = "midnightblue"))+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_blank(),
        title = element_text(face="bold", color = "red3", size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  ggtitle('Breweries by State')#Plot Heat Map


##QUESTION 2

#Merge beer and breweries
beersclean$Brew_ID=beersclean$Brewery_id
beerbreweries=merge(beersclean,breweries,by="Brew_ID",all.x = TRUE)
beerbreweries=beerbreweries%>%select(!Brewery_id)


##QUESTION 4

#Find median ABV and IBU per state
head(beerbreweries)
colnames(beerbreweries)[8]='Brewery_Name'
colnames(beerbreweries)[2]='Beer_Name'

#Gather median abv and ibu per each state
ABVIBUData=beerbreweries%>%group_by(State)%>%summarize(medianABV=median(ABV),medianIBU=median(IBU),count=n())

#barplot ABV Updated

ABVIBUData%>%
  ggplot(aes(x=reorder(State,medianABV),y=medianABV,fill=medianABV))+
  geom_bar(stat='identity', color = "grey46")+
  geom_text(aes(label  = medianABV), vjust = -1.5, size = 2.2, color = "black",fontface =  "bold")+
  ylim(0,.073)+
  ggtitle('Median ABV by State')+
  xlab('State')+
  ylab('Median ABV')+
  scale_fill_gradient2(name='Median ABV',low = "white", mid = "steelblue1", high = "midnightblue",midpoint = 0.05, limits = c(0.03,0.07), 
                       breaks=c(0.03,0.038,0.046,0.054,0.062,0.07), na.value = "grey50")+
  theme(legend.position = "none",
        title = element_text(face="bold", color = "red3", size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.title.x = element_text(face="bold", color = "red3", size = 9))


#barplot IBU Updated
ABVIBUData%>%ggplot(aes(x=reorder(State,medianIBU),y=medianIBU,fill=medianIBU))+
  geom_bar(stat='identity', color = "grey46")+
  geom_text(aes(label  = medianIBU), vjust = -1.5, size = 2.3, color = "black",fontface =  "bold")+
  ylim(0,70)+
  ggtitle('Median IBU by State')+
  xlab('State')+
  ylab('Median IBU')+
  scale_fill_gradient2(name='Median IBU',low = "white", mid = "red", high = "red4", 
                       midpoint = 40, limits = c(10,70), 
                       breaks=c(10,22,34,46,58,70), na.value = "grey50")+
  theme(legend.position = "none",
        title = element_text(face="bold", color = "midnightblue", size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(face="bold", color = "midnightblue", size = 9),
        axis.text.x = element_text(size = 7))



##QUESTION 5

#Find State w/ Max ABV and IBU Version 1
ABVIBUData%>%filter(medianABV==max(medianABV))
ABVIBUData%>%filter(medianIBU==max(medianIBU))

HighABVIBU=ABVIBUData%>%filter(medianABV==max(medianABV))
print(HighABVIBU)
formattable(HighABVIBU)
#Maine has the highest ABV and IBU

#Find State w/ Max ABV and IBU Version 2

maxABVData=beerbreweries%>%filter(ABV==max(ABV))
maxIBUData=beerbreweries%>%filter(IBU==max(IBU))
maxABVData=data.frame(maxABVData$Beer_Name, maxABVData$Style, maxABVData$Brewery_Name, maxABVData$City, maxABVData$State, maxABVData$ABV)
names(maxABVData) <- c("Beer_Name","Style","Brewery_Name","City","State","Value")
maxABVData = maxABVData%>% mutate(Type = "Maximum ABV")
maxIBUData=data.frame(maxIBUData$Beer_Name, maxIBUData$Style, maxIBUData$Brewery_Name, maxIBUData$City, maxIBUData$State, maxIBUData$IBU)
names(maxIBUData) <- c("Beer_Name","Style","Brewery_Name","City","State","Value")
maxIBUData = maxIBUData%>% mutate(Type = "Maximum IBU")
maxABVIBUData = union(maxABVData,maxIBUData)
maxABVIBUData = maxABVIBUData [, c("Type","Beer_Name","Style","Brewery_Name","City","State","Value")]
gt() %>% cols_align(maxABVIBUData, align=c("left"))
print(maxABVIBUData)

formattable(maxABVIBUData)

## KY has the highest ABV beer and OR has the Highest IBU beer

##QUESTION 6

#Summary Stats for ABV
summary(beerbreweries$ABV)

##QUESTION 7

#Scatter Plot ABV and IBU
beerbreweries%>%ggplot(aes(x=ABV,y=IBU))+
  geom_point(aes(),color='blue')+
  geom_smooth(method="lm")+
  ggtitle('Bitterness vs Alcohol Content')+
  xlab('Alcohol Content (ABV)')+
  ylab('Bitterness (IBU)')+
  theme(title = element_text(face="bold", color = "red3", size = 12),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))


#k-NN for IPAs and other ales
AleData=beerbreweries%>%filter(grepl("(Ale|IPA)",beerbreweries$Beer_Name,ignore.case = TRUE))

AleData$IPAorALE=ifelse(grepl("IPA",AleData$Beer_Name),"IPA","Ale")
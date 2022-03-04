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
ABVclean=beers%>%filter(!is.na(ABV))
IBUclean=beers%>%filter(!is.na(IBU))


##QUESTION 1 




##Slide 2

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

##End Slide 2





##Slide 3

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

##End Slide 3





##Slide 4

#Which brewery is the most productive? v2

beerbreweriesall = merge(beers,breweries, by.x="Brewery_id",by.y="Brew_ID")
colnames(beerbreweriesall)[1]='Brewery_ID'
colnames(beerbreweriesall)[2]='Beer_Name'
colnames(beerbreweriesall)[8]='Brewery_Name'

beerbreweriesallsummarybystate = beerbreweriesall%>%group_by(State)%>%
  summarize(Brewery_Count=n_distinct(Brewery_ID),Beer_Count=n_distinct(Beer_ID))
beerbreweriesallsummarybystate$Beer_Per_Brewery <- round(beerbreweriesallsummarybystate$Beer_Count/beerbreweriesallsummarybystate$Brewery_Count, digit=1)

beerbreweriesallsummarybystate%>%
  ggplot(aes(x=reorder(State,Beer_Per_Brewery),y=Beer_Per_Brewery,fill=Beer_Per_Brewery))+
  geom_bar(stat='identity', color = "grey46")+
  geom_text(aes(label  = Beer_Per_Brewery), vjust = -1.5, size = 2.2, color = "black",fontface =  "bold")+
  ylim(0,9)+
  ggtitle('Beer Produced per Brewery in State')+
  xlab('State')+
  ylab('Avg. Beer per Brewery')+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red4", 
                       midpoint = 4, limits = c(1,8), 
                       breaks=c(1,2,3,4,5,6,7,8), na.value = "grey50")+
  theme(legend.position = "none",
        title = element_text(face="bold", color = "midnightblue", size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.title.x = element_text(face="bold", color = "midnightblue", size = 9))


beerbreweriesallsummarybybrewery = beerbreweriesall%>%group_by(State,Brewery_Name)%>%
  summarize(Brewery_Count=n_distinct(Brewery_ID),Beer_Count=n_distinct(Beer_ID))
beerbreweriesallsummarybybrewery$Beer_Per_Brewery <- round(beerbreweriesallsummarybybrewery$Beer_Count/beerbreweriesallsummarybybrewery$Brewery_Count, digit=1)

Top_Brewery=beerbreweriesallsummarybybrewery%>%filter(State==" DC")
Top_Brewery_print=data.frame(Top_Brewery$State,Top_Brewery$Brewery_Name,Top_Brewery$Beer_Count)
names(Top_Brewery_print)=(c('State','Brewery Name','Beer Count'))
formattable(Top_Brewery_print)

##End Slide 4




##QUESTION 2

#Merge beer and breweries
beersclean$Brew_ID=beersclean$Brewery_id
beerbreweries=merge(beersclean,breweries,by="Brew_ID",all.x = TRUE)
beerbreweries=beerbreweries%>%select(!Brewery_id)

#Merge cleaned ABV data with breweries
ABVclean$Brew_ID=ABVclean$Brewery_id
ABVbreweries=merge(ABVclean,breweries,by="Brew_ID",all.x = TRUE)
ABVbreweries=ABVbreweries%>%select(!Brewery_id)

#Merge cleaned IBU data with breweries
IBUclean$Brew_ID=IBUclean$Brewery_id
IBUbreweries=merge(IBUclean,breweries,by="Brew_ID",all.x = TRUE)
IBUbreweries=IBUbreweries%>%select(!Brewery_id)

#Print first 6 and last 6 observations
head(beerbreweries)
tail(beerbreweries)

##QUESTION 4

#Find median ABV and IBU per state
colnames(beerbreweries)[8]='Brewery_Name'
colnames(beerbreweries)[2]='Beer_Name'

colnames(ABVbreweries)[8]='Brewery_Name'
colnames(ABVbreweries)[2]='Beer_Name'

colnames(IBUbreweries)[8]='Brewery_Name'
colnames(IBUbreweries)[2]='Beer_Name'


#Gather median abv and ibu per each state
ABVIBUData=beerbreweries%>%group_by(State)%>%summarize(medianABV=median(ABV),medianIBU=median(IBU),count=n())
ABVData=ABVbreweries%>%group_by(State)%>%summarize(medianABV=median(ABV),count=n())
IBUData=IBUbreweries%>%group_by(State)%>%summarize(medianIBU=median(IBU),count=n())





##Slide 5

#barplot ABV Updated

ABVData%>%
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
IBUData%>%ggplot(aes(x=reorder(State,medianIBU),y=medianIBU,fill=medianIBU))+
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

##End Slide 5




##QUESTION 5

#Find State w/ Max ABV and IBU Version 1
ABVData%>%filter(medianABV==max(medianABV))
IBUData%>%filter(medianIBU==max(medianIBU))
ABVIBUData%>%filter(medianIBU==max(medianIBU))


HighABVIBU=ABVIBUData%>%filter(medianABV==max(medianABV))
print(HighABVIBU)
formattable(HighABVIBU)

#Find State w/ Max ABV and IBU based on updated cleanup (separate for ABV and IBU)
HighABV=ABVData%>%filter(medianABV==max(medianABV))
print(HighABV)
formattable(HighABV)

HighIBU=IBUData%>%filter(medianIBU==max(medianIBU))
print(HighIBU)
formattable(HighIBU)

#Maine has the highest ABV and IBU when all NAs are deleted

#When accounting for all present ABV values, KY and DC have the 
#highest median ABV




##Slide 6

#Find State w/ Max ABV and IBU Version 2

maxABVData=ABVbreweries%>%filter(ABV==max(ABV))
maxIBUData=IBUbreweries%>%filter(IBU==max(IBU))
maxABVData=data.frame(maxABVData$Beer_Name, maxABVData$Style, maxABVData$Brewery_Name, maxABVData$City, maxABVData$State, maxABVData$ABV)
names(maxABVData) <- c("Beer_Name","Style","Brewery_Name","City","State","Value")
maxABVData = maxABVData%>% mutate(Type = "Maximum ABV")
maxIBUData=data.frame(maxIBUData$Beer_Name, maxIBUData$Style, maxIBUData$Brewery_Name, maxIBUData$City, maxIBUData$State, maxIBUData$IBU)
names(maxIBUData) <- c("Beer_Name","Style","Brewery_Name","City","State","Value")
maxIBUData = maxIBUData%>% mutate(Type = "Maximum IBU")
maxABVIBUData = union(maxABVData,maxIBUData)
maxABVIBUData = maxABVIBUData [, c("Type","Beer_Name","Style","Brewery_Name","City","State","Value")]
print(maxABVIBUData)
formattable(maxABVIBUData)


## CO has the highest ABV beer and OR has the Highest IBU beer


##End Slide 6


##QUESTION 6


##Slide 7

#Summary Stats for ABV
summary(beerbreweries$ABV)

#Histogram
ABVclean%>%ggplot(aes(x=ABV))+ 
  geom_histogram(bins=50,aes(),fill='blue')+
  xlab('ABV')+
  ylab('Count')+
  ggtitle('Distribution of ABV')+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(face="bold", color = "black", size = 8),
        title = element_text(face="bold", color = "red3", size = 12),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))

#Boxplot
ABVclean%>%ggplot(aes(x=ABV))+
  geom_boxplot(aes(),fill='red')+
  ylab('ABV')+
  ggtitle('Distribution of ABV')+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(face="bold", color = "black", size = 8),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face="bold", color = "red3", size = 12),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),)

SummaryABV = summary(ABVclean$ABV)

formattable(SummaryABV)

##End Slide 7




## Slide 8

#How does the correlation between ABV and IBU change as either increase?
#Scatter Plot ABV and IBU

#Check correlation at lower ABV values
beerbreweries%>%filter(ABV<0.0625)%>%
  ggplot(aes(x=ABV,y=IBU))+
  geom_point(aes(),color='blue')+
  geom_smooth(method="lm")+
  ggtitle('Bitterness vs Alcohol Content (ABV<0.0625)')+
  xlab('Alcohol Content (ABV)')+
  ylab('Bitterness (IBU)')+
  theme(title = element_text(face="bold", color = "red3", size = 12),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))
#Find R^2 at lower values
LowABV=as.data.frame(beerbreweries%>%filter(ABV<0.0625))
LmodLow=lm(IBU~ABV,LowABV)
summary(LmodLow)#r^2=0.1971

#Check correlation at higher ABV values
beerbreweries%>%filter(ABV>0.0625&ABV<0.1)%>%
  ggplot(aes(x=ABV,y=IBU))+
  geom_point(aes(),color='blue')+
  geom_smooth(method="lm")+
  ggtitle('Bitterness vs Alcohol Content (ABV>0.0625)')+
  xlab('Alcohol Content (ABV)')+
  ylab('Bitterness (IBU)')+
  theme(title = element_text(face="bold", color = "red3", size = 12),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))
#Find R^2 at higher values
HighABV=as.data.frame(beerbreweries%>%filter(ABV>0.0625))
LmodHigh=lm(IBU~ABV,HighABV)
summary(LmodHigh)#r^2=0.07411

#Find overall r^2
LmodTotal=lm(IBU~ABV,beerbreweries)
summary(LmodTotal)#r^2=0.4497

#Both high and low lines on the same graph
HighLowAbv=ifelse(beerbreweries$ABV>0.0625,'High','Low')
beerbreweries%>%mutate(HighLowAbv)%>%
  ggplot(aes(x=ABV,y=IBU))+
  geom_point(aes(),col=ifelse(beerbreweries$ABV>0.0625,'blue','red'))+
  geom_smooth(method='lm',aes(col=HighLowAbv))+
  ggtitle('Bitterness vs Alcohol Content')+
  xlab('Alcohol Content (ABV)')+
  ylab('Bitterness (IBU)')+
  theme(title = element_text(face="bold", color = "red3", size = 12),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9),
        legend.title=element_blank())

##End Slide 8


##################NOT USING##############################

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


################################################


##Slide 9

##QUESTION 8
#Filter out beers that are either an Ale or an IPA
AleData=beers%>%filter((grepl("(IPA)",beers$Style))|(grepl("(Ale)",beers$Name)))


#Classify the drinks as either Ale or IPA
AleData$IPAorALE=ifelse(grepl("IPA",AleData$Style),"IPA","Ale")
AleClean=AleData%>%filter(!(is.na(IBU)|is.na(ABV)))

#Set up Matrix to store Accuracy, Specificity,and Sensitivity values for the upcoming confusion matrix 
iterations=500
numks=30
masterAcc=matrix(nrow=iterations,ncol=numks)
masterSen=matrix(nrow=iterations,ncol=numks)
masterSpec=matrix(nrow=iterations,ncol=numks)

for (j in 1:iterations){
  #70-30 Training-Test Split
  set.seed(sample(1:100000,1))
  trainInd=sample(1:dim(AleClean)[1],round(0.7*dim(AleClean)[1]))
  trainAle=AleClean[trainInd,]
  testAle=AleClean[-trainInd,]
  
  for(i in 1:numks){
    #k-NN to predict whether the drink is an IPA or an Ale
    AlePredictions=knn(trainAle[,c('ABV','IBU')],testAle[,c('ABV','IBU')],trainAle$IPAorALE,k=i,prob=TRUE)
    AleTable=table(AlePredictions,testAle$IPAorALE)
    AleCM=confusionMatrix(AleTable)
    masterAcc[j,i]=AleCM$overall[1]
    masterSen[j,i]=AleCM$byClass[1]
    masterSpec[j,i]=AleCM$byClass[2]
  }
}

#Collect the mean stats for each k-val
meanAcc=colMeans(masterAcc)
meanSen=colMeans(masterSen)
meanSpec=colMeans(masterSpec)

#Create dataframe with all stats
AleStats=data.frame(k=1:30,Mean_Accuracy=meanAcc,Mean_Sensitivity=meanSen,Mean_Specificity=meanSpec,Sum_Stat=(meanSpec+meanSen+meanAcc))

#Tune k-val based on all three stats
HighAleStats=AleStats%>%filter(Sum_Stat==max(Sum_Stat))
formattable(HighAleStats)

#k=4 seemed to give the best balance between accuracy, sensitivity, and specificity
#I would prefer to use an odd number, but k=3,4, or 5 should provide good results regardless

#70-30 Training-Test Split
set.seed(sample(1:100000,1))
trainInd=sample(1:dim(AleClean)[1],round(0.7*dim(AleClean)[1]))
trainAle=AleClean[trainInd,]
testAle=AleClean[-trainInd,]

#3-NN to predict whether the drink is an IPA or an Ale
AlePredictions=knn(trainAle[,c('ABV','IBU')],testAle[,c('ABV','IBU')],trainAle$IPAorALE,k=3,prob=TRUE)
AleTable=table(AlePredictions,testAle$IPAorALE)
confusionMatrix(AleTable)

#Scatterplots of prediction and actual classifications
#Scatterplot of actual classifications
testAle%>%ggplot(aes(ABV,IBU,color=IPAorALE))+
  geom_point()+
  ggtitle('Bitterness vs Alcohol Content')+
  xlab('Alcohol Content (ABV)')+
  ylab('Bitterness (IBU)')+
  theme(title = element_text(face="bold", color = "red3", size = 12),
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))

##End Slide 9



##Slide 10

#Scatterplot of predicted classifications
testAle%>%mutate(AlePredictions)%>%
  ggplot(aes(ABV,IBU,color=AlePredictions))+
  geom_point()+
  ggtitle('Bitterness vs Alcohol Content')+
  xlab('Alcohol Content (ABV)')+
  ylab('Bitterness (IBU)')+
  theme(title = element_text(face="bold", color = "red3", size = 12),
        legend.title=element_blank(),
        axis.title.x = element_text(face="bold", color = "dodgerblue3", size = 9),
        axis.title.y = element_text(face="bold", color = "dodgerblue3", size = 9))

#It appears IBU and ABV levels can explain whether or not a beverage is an IPA or an ale


##End Slide 9

##################NOT USING##############################

#Additional Questions
#Which brewery is the most productive? v1
print(beerbreweries%>%group_by(Brewery_Name)%>%
        summarize(State,count=n())%>%
        filter(count==32),n=100)
#Two most productive breweries were Oskar Blues Brewery (CO) 
#and Sun King Brewing Company (IN) 
brewerydf=data.frame(c('Oskar Blues Brewery','Sun King Brewing Company'),c('CO','IN'),c(32,32))
names(brewerydf)=(c('Brewery','State','Number of Breweries'))
formattable(brewerydf)

################################################



getwd()

#home repo
# setwd("C:\\Users\\Jazzy\\OneDrive\\Documents\\SMU\\Spring '19\\Doing Data Science\\GitHub\\MSDS_DDS_Spring_2019\\Project\\Code")
#work repo
#setwd("C:/Users/jcolem230/OneDrive - Comcast/Work Keep/Me/SMU/DDS/MSDS_DDS_Spring_2019/Project/Code")


#Downloading the raw beer and brewery data
library(downloader)
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv", destfile="Breweries.csv")
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv", destfile="Beers.csv")
list.files()

#Importing the downloaded data
Breweries <- read.csv2("Breweries.csv", header = TRUE, sep = ",")
dim(Breweries)
head(Breweries)
str(Breweries)
class(Breweries)

Beer <- read.csv2("Beers.csv", header = TRUE, sep = ",")
dim(Beer)
head(Beer)
str(Beer)
class(Beer)

# install.packages("dplyr")
# install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#How many breweries are in each state?

    #count(Breweries$State)
#counting occurrences of state in dataset
BrewbyState <- Breweries %>% group_by(State) %>% count(State)
Breweries %>% group_by(State) %>% summarize(no_rows = length(State))
#remove spaces from state variable
BrewbyState$State <- gsub(" ", "", BrewbyState$State, fixed = TRUE)
#Calculating % of Total
BrewbyState$Percentage <- round((BrewbyState$n/sum(BrewbyState$n))*100,2)
#putting results into a dataframe
BrewbyState <- data.frame(BrewbyState)
names(BrewbyState)[2] <- "Count"
str(BrewbyState)
BrewbyState
###Top 10 Brewery States###
TopBrewStates <- head(BrewbyState[order(-BrewbyState$Percentage),],10)
#Reordering
BrewbyState <- BrewbyState[order(-BrewbyState$Percentage),]
#Total percentage for top 10 brewery states
sum(head(BrewbyState$Percentage,10))

#exporting dataframe to csv
write.csv(BrewbyState, file = "BreweryCount.csv")


#plot brewery count by state in descending order
ggplot(BrewbyState, aes(y=BrewbyState$Count , x=reorder(BrewbyState$State, BrewbyState$Count), fill=Count)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black")) +
  labs(title = "Brewery Count by State", y = "Count", x = "State") +
  scale_fill_gradientn(colours = rainbow(3)) +
  scale_y_continuous(expand = c(0, 0))

#plot brewery percentages by state
ggplot(BrewbyState, aes(y=BrewbyState$Percentage , x=reorder(BrewbyState$State, BrewbyState$Percentage), fill=Percentage)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black")) +
  labs(title = "Percentage of Breweries in Each State", y = "Percentage", x = "State") +
  scale_fill_gradientn(colours = rainbow(3)) +
  scale_y_continuous(expand = c(0, 0))

summary(BrewbyState)


#Merging Datasets
Beer_and_Brewery <-merge(Beer,Breweries, by.x = "Brewery_id", by.y = "Brew_ID", all.x = TRUE)
str(Beer_and_Brewery)
#Modifying variable types and updating column names
Beer_and_Brewery$State <- gsub(" ", "", Beer_and_Brewery$State, fixed = TRUE)
Beer_and_Brewery$ABV <- as.numeric(as.character(Beer_and_Brewery$ABV))
Beer_and_Brewery$Name.x <- as.character(Beer_and_Brewery$Name.x)
Beer_and_Brewery$Style <- as.character(Beer_and_Brewery$Style)
Beer_and_Brewery$Name.y <- as.character(Beer_and_Brewery$Name.y)
Beer_and_Brewery$City <- as.character(Beer_and_Brewery$City)
names(Beer_and_Brewery)[2] <- "Beer"
names(Beer_and_Brewery)[8] <- "Brewery"
str(Beer_and_Brewery)

#Beers per Brewery
summary(Beer_and_Brewery)
BeerbyState <- Beer_and_Brewery %>% group_by(State) %>% summarize(no_rows = length(State))
sum(BeerbyState$no_rows)
BeerbyState$Percentage <- round((BeerbyState$no_rows/sum(BeerbyState$no_rows))*100,2)
names(BeerbyState)[2] <- "BeersBrewed" 
BeerbyState <-data.frame(BeerbyState)
BeerbyState

#plot beer percentages by state
ggplot(BeerbyState, aes(y=BeerbyState$Percentage , x=reorder(BeerbyState$State, BeerbyState$Percentage), fill=Percentage)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black")) +
  labs(title = "Percentage of Beers Brewed in Each State", y = "Percentage", x = "State") +
  scale_fill_gradientn(colours = rainbow(3)) +
  scale_y_continuous(expand = c(0, 0))

###Top 10 Beer States###
TopBeerStates<- head(BeerbyState[order(-BeerbyState$Percentage),],10)
TopBeerStateNames <- TopBeerStates[1:10,1]


#Reordering
BeerbyState <- BeerbyState[order(-BeerbyState$Percentage),]
#Total percentage for top 10 brewery states
sum(head(BrewbyState$Percentage,10))

#plot TopBrewStates
ggplot(TopBrewStates, aes(y=TopBrewStates$Count , x=reorder(TopBrewStates$State, TopBrewStates$Count), fill=-Count)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  labs(title = "Top 10 Brewery States", y = "Total Breweries in Each State", x = "State") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 50), position = "right") +
  geom_text(aes(label = Count), hjust = -.5, size=3) #+ theme(legend.position="bottom")

#plot TopBeerStates
ggplot(TopBeerStates, aes(y=TopBeerStates$BeersBrewed , x=reorder(TopBeerStates$State, TopBeerStates$BeersBrewed), fill=-BeersBrewed)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  labs(title = "Top 10 Beer Producing States", y = "Total Beers Produced", x = "State") + 
  scale_y_continuous(expand = c(0, 0),limits = c(0, 300), position = "right") +
  geom_text(aes(label = BeersBrewed), hjust = -.5, size=3) #+ theme(legend.position="bottom")

#Plotting First 6 and Last 6 Records
head(Beer_and_Brewery,6)
tail(Beer_and_Brewery,6)
DataPreview <- data.frame(union(head(Beer_and_Brewery,6),tail(Beer_and_Brewery,6)))
DataPreview
  # library(gridExtra)
  # library(grid)
  # grid.newpage()
  # grid.table(DataPreview)

#checking row count
dim(merge(Beer,Breweries, by.x = "Brewery_id", by.y = "Brew_ID"))
dim(Beer)
dim(Breweries)
dim(Beer_and_Brewery)

#counting N/As by column
class(Beer_and_Brewery)

colSums(is.na(Beer_and_Brewery))
apply(is.na(Beer_and_Brewery), 2, sum)
str(Beer_and_Brewery)


#plot median ABV and IBU by State
    
    # mysummary<-function(x){
    #   result<-c(length(x),mean(x),sd(x),sd(x)/length(x))
    #   names(result)<-c("N","Mean","SD","SE")
    #   return(result)
    # }

medianIBU <- aggregate(IBU~State,data=Beer_and_Brewery,median)
medianABV <- aggregate(as.numeric(as.character(Beer_and_Brewery$ABV))~State,data=Beer_and_Brewery,median)
 
  #looking at them side by side to find missing state -- which is SD
paste0(medianIBU$State,medianABV$State, sep = "")
Beer_and_Brewery[which(Beer_and_Brewery$State == "SD"),]
  #merging both median vectors
medianstats <- merge(medianIBU, medianABV, by = "State", all = TRUE)
medianstats <- data.frame(medianstats)
names(medianstats)[3] <- "ABV"
#medianstats$ABV_adj <- medianstats$ABV*100
dim(medianstats)
str(medianstats)
medianstats
  #changing to long form to create side by side barplot
  # library(reshape2)
  # medianstats_long<-melt(medianstats,id.vars="State", measure.vars = c("IBU","ABV_adj"), value.name = "Medians", na.rm = FALSE)
  # medianstats_long$variable <- ifelse(medianstats_long$variable=="ABV", "ABV","IBU")
  #   #creaing side by side barplot
  # ggplot(medianstats_long,aes(y=medianstats_long$Medians, x=reorder(medianstats_long$State, medianstats_long$Medians), fill=factor(variable)))+
  #   geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  #   theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6)) +
  #   labs(title = "Median ABV and IBU by State", x = "State", y = "Median Values", fill = "Metric:")
  # #update with dual axis

#idividual bar charts for ABV and IBU
ggplot(medianstats, aes(y=medianstats$IBU, x=reorder(medianstats$State, medianstats$IBU), fill=factor(IBU)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_text(size=8, face = "bold"), axis.line.x = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Median IBU by State", x = "State", y = "Median Values")

ggplot(medianstats,aes(y=medianstats$ABV, x=reorder(medianstats$State, medianstats$ABV), fill=factor(ABV)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_text(size=8, face = "bold"), axis.line.x = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Median ABV by State", x = "State", y = "Median Values")

#Plot of top 10 Beer States Media Values
TopBeerMedians <- subset(medianstats, State %in% TopBeerStateNames)

ABVPlot<- ggplot(TopBeerMedians,aes(y=TopBeerMedians$ABV, x=reorder(TopBeerMedians$State, TopBeerMedians$ABV), fill=factor(ABV)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position="none") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, .067), position = "right") +
  labs(title = "Median ABV for Top 10 Beer Producing States", x = "State", y = "Median Values") +
  geom_text(aes(label = ABV), hjust = -.5, size=3)

IBUPlot <- ggplot(TopBeerMedians, aes(y=TopBeerMedians$IBU, x=reorder(TopBeerMedians$State, TopBeerMedians$IBU), fill=factor(IBU)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position="none") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 45), position = "right") +
  labs(title = "Median IBU for Top 10 Beer Producing States", x = "State", y = "Median Values") +
  geom_text(aes(label = IBU), hjust = -.5, size=3)

#maxIBU
summary(medianstats)

#sumary statistics
mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x),max(x),IQR(x))
  names(result)<-c("N","Mean","SD","SE","Min","Max","IQR")
  return(result)
}
sumstats <-aggregate(Beer_and_Brewery$ABV~State,data=Beer_and_Brewery,mysummary)
sumstats

summary(Beer_and_Brewery$ABV)
summary(Beer_and_Brewery$IBU)

#State with highest ABV and IBU values
maxIBUs <- aggregate(Beer_and_Brewery$IBU~State,data = Beer_and_Brewery,max)
maxABV <- aggregate(Beer_and_Brewery$ABV~State,data = Beer_and_Brewery,max)

head(maxIBUs[order(-maxIBUs$`Beer_and_Brewery$IBU`),],5)
head(maxABV[order(-maxABV$`Beer_and_Brewery$ABV`),],5)


#scatter plot to show the relationship between the bitterness of the beer and its alcoholic content
ggplot(Beer_and_Brewery, aes(x=ABV, y=IBU)) + geom_point(color="blue", size=1.5, alpha=0.3, pch=16) +
  theme_bw(base_size=16) + geom_smooth(method = "lm", se = T, color="red")




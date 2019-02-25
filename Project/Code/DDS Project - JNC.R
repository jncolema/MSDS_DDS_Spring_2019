getwd()
setwd("C:\\Users\\Jazzy\\OneDrive\\Documents\\SMU\\Spring '19\\Doing Data Science\\GitHub\\MSDS_DDS_Spring_2019\\Project\\Code")

# library(git2r)



library(downloader)
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv", destfile="Breweries.csv")
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv", destfile="Beers.csv")
list.files()

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

library(dpylr)
library(ggplot2)

#How many breweries are in each state?

count(Breweries$State)
BrewbyState <- Breweries %>% group_by(State) %>% count(State)
Breweries %>% group_by(State) %>% summarize(no_rows = length(State))
#remove spaces
BrewbyState$State <- gsub(" ", "", BrewbyState$State, fixed = TRUE)
BrewbyState <- data.frame(BrewbyState) 
str(BrewbyState)

#plot count
ggplot(BrewbyState, aes(y=BrewbyState$n , x=reorder(BrewbyState$State, BrewbyState$n))) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6)) +
  labs(title = "Breweries Count by Stae", y = "Count", x = "State")

summary(BrewbyState)
#top x states vs lowest ... by %

#Merging Datasets

Beer_and_Brewery <-merge(Beer,Breweries, by.x = "Brewery_id", by.y = "Brew_ID", all.x = TRUE)
Beer_and_Brewery$State <- gsub(" ", "", Beer_and_Brewery$State, fixed = TRUE)
Beer_and_Brewery$ABV <- as.numeric(as.character(Beer_and_Brewery$ABV))
# Beer_and_Brewery$Name.x change name and update all factor variables

union(head(Beer_and_Brewery,6),tail(Beer_and_Brewery,6))
head(Beer_and_Brewery,6)
tail(Beer_and_Brewery,6)
#The last 10 rows are all for names stating

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
dim(medianstats)
str(medianstats)
medianstats
  #changing to long form to create side by side barplot
library(reshape2)
medianstats_long<-melt(medianstats,id.vars="State", measure.vars = c("IBU","as.numeric(as.character(Beer_and_Brewery$ABV))"), value.name = "Medians", na.rm = FALSE)
medianstats_long$variable <- ifelse(medianstats_long$variable=="as.numeric(as.character(Beer_and_Brewery$ABV))", "ABV","IBU")
  #creaing side by side barplot
ggplot(medianstats_long,aes(y=medianstats_long$Medians, x=reorder(medianstats_long$State, medianstats_long$Medians), fill=factor(variable)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6)) +
  labs(title = "Median ABV and IBU by State", x = "State", y = "Median Values", fill = "Metric:")
#update with dual axis

#maxIBU

mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x),max(x),IQR(x))
  names(result)<-c("N","Mean","SD","SE","Min","Max","IQR")
  return(result)
}
sumstats <-aggregate(as.numeric(as.character(Beer_and_Brewery$ABV))~State,data=Beer_and_Brewery,mysummary)
sumstats
#get sum stats in general not jsut by each state
summary(Beer_and_Brewery$ABV)

maxIBUs <- as.numeric(as.character(Beer_and_Brewery$ABV))
maxABV <- aggregate(as.numeric(as.character(Beer_and_Brewery$ABV))~State,data = Beer_and_Brewery,max)

head(maxIBUs[order(-maxIBUs$IBU),],5)
head(maxABV[order(-maxABV$`as.numeric(as.character(Beer_and_Brewery$ABV))`),],5)
  #as.numeric(as.character(Beer_and_Brewery$ABV))

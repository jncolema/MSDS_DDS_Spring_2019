---
title: "Budweiser Competitive Analysis"
author: "Jasmine Coleman"
date: "February 28, 2019"
output:
  html_document:
    keep_md: true
---


#Purpose
####This code explores interesting findings on the US craft brewery market to help provide insight to Budweiser executives regarding future beer production.  

#Gathering the Data  
##Downloading the raw beer and brewery data  

```r
# library(downloader)
# download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv", destfile="Breweries.csv")
# download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv", destfile="Beers.csv")
```
  
##Importing the downloaded data

```r
Breweries <- read.csv2("Breweries.csv", header = TRUE, sep = ",")
dim(Breweries)
```

```
## [1] 558   4
```

```r
head(Breweries)
```

```
##   Brew_ID                      Name          City State
## 1       1        NorthGate Brewing    Minneapolis    MN
## 2       2 Against the Grain Brewery    Louisville    KY
## 3       3  Jack's Abby Craft Lagers    Framingham    MA
## 4       4 Mike Hess Brewing Company     San Diego    CA
## 5       5   Fort Point Beer Company San Francisco    CA
## 6       6     COAST Brewing Company    Charleston    SC
```

```r
str(Breweries)
```

```
## 'data.frame':	558 obs. of  4 variables:
##  $ Brew_ID: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Name   : Factor w/ 551 levels "10 Barrel Brewing Company",..: 355 12 266 319 201 136 227 477 59 491 ...
##  $ City   : Factor w/ 384 levels "Abingdon","Abita Springs",..: 228 200 122 299 300 62 91 48 152 136 ...
##  $ State  : Factor w/ 51 levels " AK"," AL"," AR",..: 24 18 20 5 5 41 6 23 23 23 ...
```

```r
class(Breweries)
```

```
## [1] "data.frame"
```

```r
Beer <- read.csv2("Beers.csv", header = TRUE, sep = ",")
dim(Beer)
```

```
## [1] 2410    7
```

```r
head(Beer)
```

```
##                  Name Beer_ID   ABV IBU Brewery_id
## 1            Pub Beer    1436  0.05  NA        409
## 2         Devil's Cup    2265 0.066  NA        178
## 3 Rise of the Phoenix    2264 0.071  NA        178
## 4            Sinister    2263  0.09  NA        178
## 5       Sex and Candy    2262 0.075  NA        178
## 6        Black Exodus    2261 0.077  NA        178
##                            Style Ounces
## 1            American Pale Lager     12
## 2        American Pale Ale (APA)     12
## 3                   American IPA     12
## 4 American Double / Imperial IPA     12
## 5                   American IPA     12
## 6                  Oatmeal Stout     12
```

```r
str(Beer)
```

```
## 'data.frame':	2410 obs. of  7 variables:
##  $ Name      : Factor w/ 2305 levels "#001 Golden Amber Lager",..: 1638 577 1705 1842 1819 268 1160 758 1093 486 ...
##  $ Beer_ID   : int  1436 2265 2264 2263 2262 2261 2260 2259 2258 2131 ...
##  $ ABV       : Factor w/ 75 levels "","0.001","0.027",..: 21 37 42 61 46 48 16 36 26 57 ...
##  $ IBU       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ Brewery_id: int  409 178 178 178 178 178 178 178 178 178 ...
##  $ Style     : Factor w/ 100 levels "","Abbey Single Ale",..: 19 18 16 12 16 80 18 22 18 12 ...
##  $ Ounces    : Factor w/ 7 levels "12","16","16.9",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
class(Beer)
```

```
## [1] "data.frame"
```
  
#Analyzing the Data  
##How many breweries are in each state?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

#Counting occurrences for each state in dataset
BrewbyState <- Breweries %>% group_by(State) %>% count(State)

#Remove spaces from state variable
BrewbyState$State <- gsub(" ", "", BrewbyState$State, fixed = TRUE)

#Calculating % of total breweries in each state
BrewbyState$Percentage <- round((BrewbyState$n/sum(BrewbyState$n))*100,2)

#Putting results into a dataframe
BrewbyState <- data.frame(BrewbyState)
names(BrewbyState)[2] <- "Count"
str(BrewbyState)
```

```
## 'data.frame':	51 obs. of  3 variables:
##  $ State     : chr  "AK" "AL" "AR" "AZ" ...
##  $ Count     : int  7 3 2 11 39 47 8 1 2 15 ...
##  $ Percentage: num  1.25 0.54 0.36 1.97 6.99 8.42 1.43 0.18 0.36 2.69 ...
```

```r
summary(BrewbyState)
```

```
##     State               Count         Percentage   
##  Length:51          Min.   : 1.00   Min.   :0.180  
##  Class :character   1st Qu.: 3.50   1st Qu.:0.630  
##  Mode  :character   Median : 7.00   Median :1.250  
##                     Mean   :10.94   Mean   :1.962  
##                     3rd Qu.:16.00   3rd Qu.:2.870  
##                     Max.   :47.00   Max.   :8.420
```

```r
###Top 10 Brewery States###
TopBrewStates <- head(BrewbyState[order(-BrewbyState$Percentage),],10)

#Sorting table in descending order by percentage of total breweries
BrewbyState <- BrewbyState[order(-BrewbyState$Percentage),]
#Precentage of breweries located in 10 states with the most breweries
sum(head(BrewbyState$Percentage,10))
```

```
## [1] 51.6
```

```r
#exporting dataframe to csv file for Tableau heatmapping
#write.csv(BrewbyState, file = "BreweryCount.csv")
```

###This table displays how many breweries are in each state:

```r
BrewbyState
```

```
##    State Count Percentage
## 6     CO    47       8.42
## 5     CA    39       6.99
## 23    MI    32       5.73
## 38    OR    29       5.20
## 44    TX    28       5.02
## 39    PA    25       4.48
## 20    MA    23       4.12
## 48    WA    23       4.12
## 16    IN    22       3.94
## 49    WI    20       3.58
## 28    NC    19       3.41
## 15    IL    18       3.23
## 35    NY    16       2.87
## 46    VA    16       2.87
## 10    FL    15       2.69
## 36    OH    15       2.69
## 24    MN    12       2.15
## 4     AZ    11       1.97
## 47    VT    10       1.79
## 22    ME     9       1.61
## 25    MO     9       1.61
## 27    MT     9       1.61
## 7     CT     8       1.43
## 1     AK     7       1.25
## 11    GA     7       1.25
## 21    MD     7       1.25
## 37    OK     6       1.08
## 13    IA     5       0.90
## 14    ID     5       0.90
## 19    LA     5       0.90
## 30    NE     5       0.90
## 40    RI     5       0.90
## 12    HI     4       0.72
## 18    KY     4       0.72
## 33    NM     4       0.72
## 41    SC     4       0.72
## 45    UT     4       0.72
## 51    WY     4       0.72
## 2     AL     3       0.54
## 17    KS     3       0.54
## 31    NH     3       0.54
## 32    NJ     3       0.54
## 43    TN     3       0.54
## 3     AR     2       0.36
## 9     DE     2       0.36
## 26    MS     2       0.36
## 34    NV     2       0.36
## 8     DC     1       0.18
## 29    ND     1       0.18
## 42    SD     1       0.18
## 50    WV     1       0.18
```

###Plots visualizing brewery count by state:

```r
#Plots brewery count by state in descending order
ggplot(BrewbyState, aes(y=BrewbyState$Count , x=reorder(BrewbyState$State, BrewbyState$Count), fill=Count)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black")) +
  labs(title = "Brewery Count by State", y = "Count", x = "State") +
  scale_fill_gradientn(colours = rainbow(3)) +
  scale_y_continuous(expand = c(0, 0))
```

![](DDS_Project_Code_-_JNC_files/figure-html/Brew Plots-1.png)<!-- -->

```r
#Plot brewery percentages by state
ggplot(BrewbyState, aes(y=BrewbyState$Percentage , x=reorder(BrewbyState$State, BrewbyState$Percentage), fill=Percentage)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black")) +
  labs(title = "Percentage of Breweries in Each State", y = "Percentage", x = "State") +
  scale_fill_gradientn(colours = rainbow(3)) +
  scale_y_continuous(expand = c(0, 0))
```

![](DDS_Project_Code_-_JNC_files/figure-html/Brew Plots-2.png)<!-- -->

##Merging Beer and Brewery Datasets

```r
Beer_and_Brewery <-merge(Beer,Breweries, by.x = "Brewery_id", by.y = "Brew_ID", all.x = TRUE)
str(Beer_and_Brewery)
```

```
## 'data.frame':	2410 obs. of  10 variables:
##  $ Brewery_id: int  1 1 1 1 1 1 2 2 2 2 ...
##  $ Name.x    : Factor w/ 2305 levels "#001 Golden Amber Lager",..: 802 1258 2185 1640 1926 1525 458 1218 43 71 ...
##  $ Beer_ID   : int  2692 2691 2690 2689 2688 2687 2686 2685 2684 2683 ...
##  $ ABV       : Factor w/ 75 levels "","0.001","0.027",..: 16 20 19 31 31 27 51 74 48 13 ...
##  $ IBU       : int  50 26 19 38 25 47 68 80 25 42 ...
##  $ Style     : Factor w/ 100 levels "","Abbey Single Ale",..: 16 77 48 83 22 57 12 46 77 18 ...
##  $ Ounces    : Factor w/ 7 levels "12","16","16.9",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ Name.y    : Factor w/ 551 levels "10 Barrel Brewing Company",..: 355 355 355 355 355 355 12 12 12 12 ...
##  $ City      : Factor w/ 384 levels "Abingdon","Abita Springs",..: 228 228 228 228 228 228 200 200 200 200 ...
##  $ State     : Factor w/ 51 levels " AK"," AL"," AR",..: 24 24 24 24 24 24 18 18 18 18 ...
```

```r
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
```

```
## 'data.frame':	2410 obs. of  10 variables:
##  $ Brewery_id: int  1 1 1 1 1 1 2 2 2 2 ...
##  $ Beer      : chr  "Get Together" "Maggie's Leap" "Wall's End" "Pumpion" ...
##  $ Beer_ID   : int  2692 2691 2690 2689 2688 2687 2686 2685 2684 2683 ...
##  $ ABV       : num  0.045 0.049 0.048 0.06 0.06 0.056 0.08 0.125 0.077 0.042 ...
##  $ IBU       : int  50 26 19 38 25 47 68 80 25 42 ...
##  $ Style     : chr  "American IPA" "Milk / Sweet Stout" "English Brown Ale" "Pumpkin Ale" ...
##  $ Ounces    : Factor w/ 7 levels "12","16","16.9",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ Brewery   : chr  "NorthGate Brewing " "NorthGate Brewing " "NorthGate Brewing " "NorthGate Brewing " ...
##  $ City      : chr  "Minneapolis" "Minneapolis" "Minneapolis" "Minneapolis" ...
##  $ State     : chr  "MN" "MN" "MN" "MN" ...
```

###This is a sample of the merged dataset:

```r
#Plotting First 6 and Last 6 Records

##First 6 rows of data
head(Beer_and_Brewery,6)
```

```
##   Brewery_id          Beer Beer_ID   ABV IBU
## 1          1  Get Together    2692 0.045  50
## 2          1 Maggie's Leap    2691 0.049  26
## 3          1    Wall's End    2690 0.048  19
## 4          1       Pumpion    2689 0.060  38
## 5          1    Stronghold    2688 0.060  25
## 6          1   Parapet ESB    2687 0.056  47
##                                 Style Ounces            Brewery
## 1                        American IPA     16 NorthGate Brewing 
## 2                  Milk / Sweet Stout     16 NorthGate Brewing 
## 3                   English Brown Ale     16 NorthGate Brewing 
## 4                         Pumpkin Ale     16 NorthGate Brewing 
## 5                     American Porter     16 NorthGate Brewing 
## 6 Extra Special / Strong Bitter (ESB)     16 NorthGate Brewing 
##          City State
## 1 Minneapolis    MN
## 2 Minneapolis    MN
## 3 Minneapolis    MN
## 4 Minneapolis    MN
## 5 Minneapolis    MN
## 6 Minneapolis    MN
```

```r
##Last 6 rows of data
tail(Beer_and_Brewery,6)
```

```
##      Brewery_id                      Beer Beer_ID   ABV IBU
## 2405        556             Pilsner Ukiah      98 0.055  NA
## 2406        557  Heinnieweisse Weissebier      52 0.049  NA
## 2407        557           Snapperhead IPA      51 0.068  NA
## 2408        557         Moo Thunder Stout      50 0.049  NA
## 2409        557         Porkslap Pale Ale      49 0.043  NA
## 2410        558 Urban Wilderness Pale Ale      30 0.049  NA
##                        Style Ounces                       Brewery
## 2405         German Pilsener     12         Ukiah Brewing Company
## 2406              Hefeweizen     12       Butternuts Beer and Ale
## 2407            American IPA     12       Butternuts Beer and Ale
## 2408      Milk / Sweet Stout     12       Butternuts Beer and Ale
## 2409 American Pale Ale (APA)     12       Butternuts Beer and Ale
## 2410        English Pale Ale     12 Sleeping Lady Brewing Company
##               City State
## 2405         Ukiah    CA
## 2406 Garrattsville    NY
## 2407 Garrattsville    NY
## 2408 Garrattsville    NY
## 2409 Garrattsville    NY
## 2410     Anchorage    AK
```

```r
##Plotting First and Last 6 Rows of data
DataPreview <- data.frame(union(head(Beer_and_Brewery,6),tail(Beer_and_Brewery,6)))
DataPreview
```

```
##    Brewery_id                      Beer Beer_ID   ABV IBU
## 1           1              Get Together    2692 0.045  50
## 2           1             Maggie's Leap    2691 0.049  26
## 3           1                Wall's End    2690 0.048  19
## 4           1                   Pumpion    2689 0.060  38
## 5           1                Stronghold    2688 0.060  25
## 6           1               Parapet ESB    2687 0.056  47
## 7         556             Pilsner Ukiah      98 0.055  NA
## 8         557  Heinnieweisse Weissebier      52 0.049  NA
## 9         557           Snapperhead IPA      51 0.068  NA
## 10        557         Moo Thunder Stout      50 0.049  NA
## 11        557         Porkslap Pale Ale      49 0.043  NA
## 12        558 Urban Wilderness Pale Ale      30 0.049  NA
##                                  Style Ounces
## 1                         American IPA     16
## 2                   Milk / Sweet Stout     16
## 3                    English Brown Ale     16
## 4                          Pumpkin Ale     16
## 5                      American Porter     16
## 6  Extra Special / Strong Bitter (ESB)     16
## 7                      German Pilsener     12
## 8                           Hefeweizen     12
## 9                         American IPA     12
## 10                  Milk / Sweet Stout     12
## 11             American Pale Ale (APA)     12
## 12                    English Pale Ale     12
##                          Brewery          City State
## 1             NorthGate Brewing    Minneapolis    MN
## 2             NorthGate Brewing    Minneapolis    MN
## 3             NorthGate Brewing    Minneapolis    MN
## 4             NorthGate Brewing    Minneapolis    MN
## 5             NorthGate Brewing    Minneapolis    MN
## 6             NorthGate Brewing    Minneapolis    MN
## 7          Ukiah Brewing Company         Ukiah    CA
## 8        Butternuts Beer and Ale Garrattsville    NY
## 9        Butternuts Beer and Ale Garrattsville    NY
## 10       Butternuts Beer and Ale Garrattsville    NY
## 11       Butternuts Beer and Ale Garrattsville    NY
## 12 Sleeping Lady Brewing Company     Anchorage    AK
```

###Plots visualizing beer count by state:

```r
#Producing Table of Beers per State
BeerbyState <- Beer_and_Brewery %>% group_by(State) %>% summarize(no_rows = length(State))
BeerbyState$Percentage <- round((BeerbyState$no_rows/sum(BeerbyState$no_rows))*100,2)
names(BeerbyState)[2] <- "BeersBrewed" 
BeerbyState <-data.frame(BeerbyState)
BeerbyState
```

```
##    State BeersBrewed Percentage
## 1     AK          25       1.04
## 2     AL          10       0.41
## 3     AR           5       0.21
## 4     AZ          47       1.95
## 5     CA         183       7.59
## 6     CO         265      11.00
## 7     CT          27       1.12
## 8     DC           8       0.33
## 9     DE           2       0.08
## 10    FL          58       2.41
## 11    GA          16       0.66
## 12    HI          27       1.12
## 13    IA          30       1.24
## 14    ID          30       1.24
## 15    IL          91       3.78
## 16    IN         139       5.77
## 17    KS          23       0.95
## 18    KY          21       0.87
## 19    LA          19       0.79
## 20    MA          82       3.40
## 21    MD          21       0.87
## 22    ME          27       1.12
## 23    MI         162       6.72
## 24    MN          55       2.28
## 25    MO          42       1.74
## 26    MS          11       0.46
## 27    MT          40       1.66
## 28    NC          59       2.45
## 29    ND           3       0.12
## 30    NE          25       1.04
## 31    NH           8       0.33
## 32    NJ           8       0.33
## 33    NM          14       0.58
## 34    NV          11       0.46
## 35    NY          74       3.07
## 36    OH          49       2.03
## 37    OK          19       0.79
## 38    OR         125       5.19
## 39    PA         100       4.15
## 40    RI          27       1.12
## 41    SC          14       0.58
## 42    SD           7       0.29
## 43    TN           6       0.25
## 44    TX         130       5.39
## 45    UT          26       1.08
## 46    VA          40       1.66
## 47    VT          27       1.12
## 48    WA          68       2.82
## 49    WI          87       3.61
## 50    WV           2       0.08
## 51    WY          15       0.62
```

```r
#Plotting beer production percentages by state
ggplot(BeerbyState, aes(y=BeerbyState$Percentage , x=reorder(BeerbyState$State, BeerbyState$Percentage), fill=Percentage)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black")) +
  labs(title = "Percentage of Beers Brewed in Each State", y = "Percentage", x = "State") +
  scale_fill_gradientn(colours = rainbow(3)) +
  scale_y_continuous(expand = c(0, 0))
```

![](DDS_Project_Code_-_JNC_files/figure-html/Beer Count-1.png)<!-- -->

```r
###Top 10 Beer States###
TopBeerStates<- head(BeerbyState[order(-BeerbyState$Percentage),],10)
TopBeerStateNames <- TopBeerStates[1:10,1]

#Sorting Beer Total by State Table
BeerbyState <- BeerbyState[order(-BeerbyState$Percentage),]
#Total percentage for top 10 brewery states
sum(head(BrewbyState$Percentage,10))
```

```
## [1] 51.6
```
####Approximately 51% of all craft beer is produced in only 10 states.

```r
#Plotting TopBrewStates
ggplot(TopBrewStates, aes(y=TopBrewStates$Count , x=reorder(TopBrewStates$State, TopBrewStates$Count), fill=-Count)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  labs(title = "Top 10 Brewery States", y = "Total Breweries in Each State", x = "State") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 50), position = "right") +
  geom_text(aes(label = Count), hjust = -.5, size=3) #+ theme(legend.position="bottom")
```

![](DDS_Project_Code_-_JNC_files/figure-html/Top10 Plots-1.png)<!-- -->

```r
#Plotting TopBeerStates
ggplot(TopBeerStates, aes(y=TopBeerStates$BeersBrewed , x=reorder(TopBeerStates$State, TopBeerStates$BeersBrewed), fill=-BeersBrewed)) + geom_col() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  labs(title = "Top 10 Beer Producing States", y = "Total Beers Produced", x = "State") + 
  scale_y_continuous(expand = c(0, 0),limits = c(0, 300), position = "right") +
  geom_text(aes(label = BeersBrewed), hjust = -.5, size=3) #+ theme(legend.position="bottom")
```

![](DDS_Project_Code_-_JNC_files/figure-html/Top10 Plots-2.png)<!-- -->

##Idenifying Any Missing Values

```r
#Counting N/As by column
apply(is.na(Beer_and_Brewery), 2, sum)
```

```
## Brewery_id       Beer    Beer_ID        ABV        IBU      Style 
##          0          0          0         62       1005          0 
##     Ounces    Brewery       City      State 
##          0          0          0          0
```

```r
#Calculaing Percentage of N/As
NAs <- colSums(is.na(Beer_and_Brewery))
PercentNA <- (NAs/2410)*100
NAs
```

```
## Brewery_id       Beer    Beer_ID        ABV        IBU      Style 
##          0          0          0         62       1005          0 
##     Ounces    Brewery       City      State 
##          0          0          0          0
```

```r
PercentNA
```

```
## Brewery_id       Beer    Beer_ID        ABV        IBU      Style 
##   0.000000   0.000000   0.000000   2.572614  41.701245   0.000000 
##     Ounces    Brewery       City      State 
##   0.000000   0.000000   0.000000   0.000000
```
####There are 62 missing values for ABV (2.6%) and 1005 missing values for IBU (41.7%).

##Median Alcohol Content and Int'l Bitterness Units by State

```r
#Calculating Median Values
medianIBU <- aggregate(IBU~State,data=Beer_and_Brewery,median)
medianABV <- aggregate(as.numeric(as.character(Beer_and_Brewery$ABV))~State,data=Beer_and_Brewery,median)

#Merging both median vectors into one dataframe
medianstats <- merge(medianIBU, medianABV, by = "State", all = TRUE)
medianstats <- data.frame(medianstats)
names(medianstats)[3] <- "ABV"
dim(medianstats)
```

```
## [1] 51  3
```

```r
str(medianstats)
```

```
## 'data.frame':	51 obs. of  3 variables:
##  $ State: chr  "AK" "AL" "AR" "AZ" ...
##  $ IBU  : num  46 43 39 20.5 42 40 29 47.5 52 55 ...
##  $ ABV  : num  0.056 0.06 0.052 0.055 0.058 0.0605 0.06 0.0625 0.055 0.057 ...
```

```r
medianstats
```

```
##    State  IBU    ABV
## 1     AK 46.0 0.0560
## 2     AL 43.0 0.0600
## 3     AR 39.0 0.0520
## 4     AZ 20.5 0.0550
## 5     CA 42.0 0.0580
## 6     CO 40.0 0.0605
## 7     CT 29.0 0.0600
## 8     DC 47.5 0.0625
## 9     DE 52.0 0.0550
## 10    FL 55.0 0.0570
## 11    GA 55.0 0.0550
## 12    HI 22.5 0.0540
## 13    IA 26.0 0.0555
## 14    ID 39.0 0.0565
## 15    IL 30.0 0.0580
## 16    IN 33.0 0.0580
## 17    KS 20.0 0.0500
## 18    KY 31.5 0.0625
## 19    LA 31.5 0.0520
## 20    MA 35.0 0.0540
## 21    MD 29.0 0.0580
## 22    ME 61.0 0.0510
## 23    MI 35.0 0.0620
## 24    MN 44.5 0.0560
## 25    MO 24.0 0.0520
## 26    MS 45.0 0.0580
## 27    MT 40.0 0.0550
## 28    NC 33.5 0.0570
## 29    ND 32.0 0.0500
## 30    NE 35.0 0.0560
## 31    NH 48.5 0.0550
## 32    NJ 34.5 0.0460
## 33    NM 51.0 0.0620
## 34    NV 41.0 0.0600
## 35    NY 47.0 0.0550
## 36    OH 40.0 0.0580
## 37    OK 35.0 0.0600
## 38    OR 40.0 0.0560
## 39    PA 30.0 0.0570
## 40    RI 24.0 0.0550
## 41    SC 30.0 0.0550
## 42    SD   NA 0.0600
## 43    TN 37.0 0.0570
## 44    TX 33.0 0.0550
## 45    UT 34.0 0.0400
## 46    VA 42.0 0.0565
## 47    VT 30.0 0.0550
## 48    WA 38.0 0.0555
## 49    WI 19.0 0.0520
## 50    WV 57.5 0.0620
## 51    WY 21.0 0.0500
```
####There is no IBU data available for craft beers produced in South Dakota.

```r
#Bar Chart for Median IBU by State
ggplot(medianstats, aes(y=medianstats$IBU, x=reorder(medianstats$State, medianstats$IBU), fill=factor(IBU)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_text(size=8, face = "bold"), axis.line.x = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Median IBU by State", x = "State", y = "Median Values")
```

```
## Warning: Removed 1 rows containing missing values (geom_col).
```

![](DDS_Project_Code_-_JNC_files/figure-html/Median Barcharts-1.png)<!-- -->

```r
#Bar Chart for Median ABV by State
ggplot(medianstats,aes(y=medianstats$ABV, x=reorder(medianstats$State, medianstats$ABV), fill=factor(ABV)))+
  geom_col(position="dodge", na.rm = FALSE) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(size=8, face = "bold"), axis.text.x = element_text(size=8, face = "bold"), axis.line.x = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Median ABV by State", x = "State", y = "Median Values")
```

![](DDS_Project_Code_-_JNC_files/figure-html/Median Barcharts-2.png)<!-- -->

####Plots of Top 10 Beer States' Median Values

```r
#Subsetting data for top 10 states producing the most beer
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

ABVPlot
```

![](DDS_Project_Code_-_JNC_files/figure-html/Top Median Plots-1.png)<!-- -->

```r
IBUPlot
```

![](DDS_Project_Code_-_JNC_files/figure-html/Top Median Plots-2.png)<!-- -->

###States Producing Beer with Maximum IBU and ABV Measurements

```r
#State with highest ABV and IBU values
maxIBUs <- aggregate(Beer_and_Brewery$IBU~State,data = Beer_and_Brewery,max)
maxABV <- aggregate(Beer_and_Brewery$ABV~State,data = Beer_and_Brewery,max)

head(maxIBUs[order(-maxIBUs$`Beer_and_Brewery$IBU`),],5)
```

```
##    State Beer_and_Brewery$IBU
## 38    OR                  138
## 45    VA                  135
## 20    MA                  130
## 36    OH                  126
## 24    MN                  120
```

```r
head(maxABV[order(-maxABV$`Beer_and_Brewery$ABV`),],5)
```

```
##    State Beer_and_Brewery$ABV
## 6     CO                0.128
## 18    KY                0.125
## 16    IN                0.120
## 35    NY                0.100
## 5     CA                0.099
```
####Oregon produces beer with the highest level of bitterness at 138 IBUs.
####Colorado produces beer with the highest alcohol content at 12.8% ABV.  
  
###ABV Summary Statistics

```r
summary(Beer_and_Brewery$ABV)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00100 0.05000 0.05600 0.05977 0.06700 0.12800      62
```
###IBU Summary Statistics

```r
summary(Beer_and_Brewery$IBU)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    4.00   21.00   35.00   42.71   64.00  138.00    1005
```

###Relationship Between Alcohol Content and Level of Bitterness

```r
#scatter plot to show the relationship between the bitterness of the beer and its alcoholic content
ggplot(Beer_and_Brewery, aes(x=ABV, y=IBU)) + geom_point(color="blue", size=1.5, alpha=0.3, pch=16) +
  theme_bw(base_size=16) + geom_smooth(method = "lm", se = T, color="red")
```

```
## Warning: Removed 1005 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 1005 rows containing missing values (geom_point).
```

![](DDS_Project_Code_-_JNC_files/figure-html/Scatterplot-1.png)<!-- -->

```r
#Correlation Test
cor.test(Beer_and_Brewery$ABV,Beer_and_Brewery$IBU, alternative = "two.sided", method = "pearson", conf.level = 0.95)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Beer_and_Brewery$ABV and Beer_and_Brewery$IBU
## t = 33.863, df = 1403, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6407982 0.6984238
## sample estimates:
##       cor 
## 0.6706215
```

```r
#Pearson's r
cor(Beer_and_Brewery$ABV,Beer_and_Brewery$IBU, use="complete.obs", method = "pearson")
```

```
## [1] 0.6706215
```

```r
#Covariance
cov(Beer_and_Brewery$ABV,Beer_and_Brewery$IBU, use="complete.obs", method = "pearson")
```

```
## [1] 0.2363008
```
####There is a strong positive linear relationship between ABV and IBU (r = 0.67, *p* = 2.2e-16 from Pearson's r) . As the alcohol content of beer increases, the level of bitterness in the beer also tends to increase. 

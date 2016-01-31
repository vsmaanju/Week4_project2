Major Weather Events in USA and Their Effects in Human Life and Economy
========================================================

### Vijender Singh   
#### Jan2015   

SYNOPSIS
------------
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This document involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database between 1950 and 2011. In general, Tornado is most harmful event with respect to population health and economic problems for communities and municipalities

### DATA PROCESSING  
```{r}
knitr::opts_chunk$set(echo = TRUE, results = "hide")
# Set some variables for the download
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
compressedFile <- "./data/StormData.csv.bz2"
stormDataFile <- "./data/StormData.csv"

```

The document need the R libraries: Plyr and ggplot2.
```{r}
require(ggplot)
require(plyr)
require(knitr)
require(dplyr)
```

DATA SOURCE: We use the data file 'repdata_data_StormData.csv.bz2' that downloaded from this link: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

#### Loading Data  
```{r}
stormData <- read.csv(stormDataFile)
# Make a tidy data set with just the columns we will
# need for our two questions

# Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health? (FATALITIES & INJURIES)
# Across the United States, which types of events have the greatest economic consequences? (PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
storm_damage <- data.frame(EVTYPE=stormData$EVTYPE, FATALITIES=stormData$FATALITIES, INJURIES=stormData$INJURIES, PROPDMG=stormData$PROPDMG, PROPDMGEXP=stormData$PROPDMGEXP, CROPDMG=stormData$CROPDMG, CROPDMGEXP=stormData$CROPDMGEXP)
```

#### DATA CLEANING AND TIDYING  
```{r}
# * in order to eliminate the wrong inputs in PROPDMGEXP and CROPDMGEXP, it was created additional columns with zero and # added values only for the letters H = Hundred, K = Thousands M = Millions and B  = Billions
```

```{r}
storm_damage$PROP_US <- 0
storm_damage$CROP_US <- 0
storm_damage$damage  <- 0
storm_damage$health  <- 0
```

```{r}
storm_damage$PROP_US <- ifelse(storm_damage$PROPDMGEXP =="H" | storm_damage$PROPDMGEXP =="h",
                               storm_damage$PROPDMG*0.0000001, storm_damage$PROP_US)
storm_damage$CROP_US <- ifelse(storm_damage$CROPDMGEXP =="H"|  storm_damage$CROPDMGEXP =="h",
                               storm_damage$CROPDMG*0.0000001, storm_damage$CROP_US)

storm_damage$PROP_US <- ifelse(storm_damage$PROPDMGEXP =="K"|  storm_damage$PROPDMGEXP =="k",
                               storm_damage$PROPDMG*0.000001,  storm_damage$PROP_US)
storm_damage$CROP_US <- ifelse(storm_damage$CROPDMGEXP =="K"|  storm_damage$CROPDMGEXP =="k",
                               storm_damage$CROPDMG*0.000001,  storm_damage$CROP_US)

storm_damage$PROP_US <- ifelse(storm_damage$PROPDMGEXP =="M"|  storm_damage$PROPDMGEXP =="m", 
                               storm_damage$PROPDMG*0.001,     storm_damage$PROP_US)
storm_damage$CROP_US <- ifelse(storm_damage$CROPDMGEXP =="M"|  storm_damage$CROPDMGEXP =="m", 
                               storm_damage$CROPDMG*0.001,     storm_damage$CROP_US)

storm_damage$PROP_US <- ifelse(storm_damage$PROPDMGEXP =="B"|  storm_damage$PROPDMGEXP =="b", 
                               storm_damage$PROPDMG*1,         storm_damage$PROP_US)
storm_damage$CROP_US <- ifelse(storm_damage$CROPDMGEXP =="B"|  storm_damage$CROPDMGEXP =="b",
                               storm_damage$CROPDMG*1,         storm_damage$CROP_US)
```

### Data Consolidating: Fatalities/Injuries and Properties Damages/Crops Damages 
 
```{r}

storm_damage$health <- storm_damage$FATALITIES + storm_damage$INJURIES
storm_damage$damage <- storm_damage$PROP_US + storm_damage$CROP_US
```

### Data Aggregating: Health/Type of Occurrence and Damage/Type of Occurrence.  

```{r}

health <- aggregate(storm_damage$health, by=list(storm_damage$EVTYPE), FUN = sum)
health <- arrange(health, desc(x))
health <- head(health,10)
health <- transform( health, Group.1 = reorder(Group.1, order(x, decreasing =TRUE)))
health <- select(health, Event_Type = Group.1, Number_of_Injuries= x)
```

```{r}
storm_PDMG <- aggregate(storm_damage$damage, by=list(storm_damage$EVTYPE), FUN = sum)
storm_PDMG <- arrange(storm_PDMG, desc(x))
storm_PDMG <- head(storm_PDMG,10)
storm_PDMG <- transform( storm_PDMG, Group.1 = reorder(Group.1, order(x, decreasing =TRUE)))
storm_PDMG <- select(storm_PDMG, Event_Type = Group.1, Economic_Impact= x)
```

RESULTS 
=======================

```{r}
head(health,10)
```

```{r}
head(storm_PDMG,10)
```

### Creating bar plots utilizing ggplot  

```{r}
g<- ggplot(health, aes(Event_Type, Number_of_Injuries)) + 
    labs(title="Total Fatalities & Injuries") +
    xlab("") + ylab("Number of injuries")
plot1<- g + geom_bar(colour="red", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

```{r}
g<- ggplot(storm_PDMG, aes(Event_Type, Economic_Impact)) + 
    labs(title="Total Properties & Crop Damages") +
    xlab("") + ylab("U$ Billions")
plot2<- g + geom_bar(colour="red", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```


#### Bar plot of the 10 most important events in United States  

```{r}
print(plot1)
```
Please click the link for figure/plot.

[plot1][id1]
[id1]:https://github.com/vsmaanju/Week4_project2/blob/master/Plots/PLot1.png
```{r}
print(plot2)
```
Please click the link for figure/plot.
[plot1][id2]

[id2]:https://github.com/vsmaanju/Week4_project2/blob/master/Plots/Plot2.jpg

Results
--------------

Tornadoes are the major weather event in US, impacting fatalities and injuries which sum 96.979 cases in these period of analisys, followed by Excessive Heat with 8.428 cases. Related to the Properties and Crops damages it is possible to see that Floods have the major economic impact U$ 150.3 Billions followed by Hurricanes/Typhoons with the expressive value of U$ 71.9 billions.

References
-----------------
The following sources were used in this analysis:

Storm Data

National Weather Service Storm Data Documentation

National Climatic Data Center Storm Events FAQ

How To Handle Exponent Value of PROPDMGEXP and CROPDMGEXP

Data Visualization with ggplot2 Cheat Sheet

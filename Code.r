setwd('D:/02 Coursera/02 R/01 Johns Hopkings-Coursera/05 Reproducible Research')
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = "StormData.csv.bz2")
RawData <- read.csv(bzfile('StormData.csv.bz2'), sep = ',', header = TRUE,na.strings = 'NA')
head(RawData,n=5)
str(RawData)
##EVTYPE variable that contains all the disasters
## which EVTYPE	caused most of the fatalities or injuries
events <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme cold/Wind Chill", "Flash Flood", "Flood", "Freezing", "Frost/Freeze", "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane/Typhoon", "Ice Storm", "Lakeshore Flood", "Lake-Effect Snow", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
events_2 <- c("Astronomical Low Tide|Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme cold/Wind Chill|Extreme Cold|Wind Chill", "Flash Flood", "Flood", "Freezing", "Frost/Freeze|Frost|Freeze", "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane/Typhoon|Hurricane|Typhoon", "Ice Storm", "Lakeshore Flood", "Lake-Effect Snow", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind|Marine tstm Wind", "Rip Current", "Seiche", "Sleet", "Storm Tide", "Strong Wind", "Thunderstorm Wind|tstm wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
#to indentify the labels of thousands, millions, etc.
unique(RawData$PROPDMGEXP)
unique(RawData$CROPDMGEXP)
options(scipen = 999) #force fixed notation instead of scientific
## Doing this does not guarantee that the variables will have this structure
EVTYPE <- character()
FATALITIES <- numeric()
INJURIES <- numeric()
PROPDMG <- numeric()
PROPDMGEXP <- character()
CROPDMG <- numeric()
CROPDMGEXP <- character()
##Creating an empty data frame (a clean table) using this names as column names
ProcData <- data.frame(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
for (i in 1:length(events)){
	## matching the events_2 with the names in EVTYPE without being case sensitive and return all the columns
	rows <- RawData[grep(events_2[i],RawData$EVTYPE,ignore.case = TRUE),]
	rows <- rows[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")] ## give me all the rows of the previous line but only with these 7 columns
	Names <- c(rep(events[i],nrow(rows))) ## i want you to repeat the names given in events for all the rows. It's basically a rename of all the variables (902,297 obs)
	rows <- cbind(rows,Names) ## binding all the 902,297 names to the 902,297 observations
	ProcData <- rbind(ProcData,rows) ## binding the empty data frame with the 902,297 rows from the previous step.
}

##create a column with the powers
##ProcData$PROPDMGEXP_POWER <- rep(0,nrow(ProcData)) and ProcData$CROPDMGEXP_POWER <- rep(0,nrow(ProcData)) 

## use ifelse to replace letters for numbers -> TOO INEFFICIENT

##-----------------------------------------------------------------------------------------------------------
## To do this you need to check if the variables are characters otherwise R will return an error like 'missing values are not allowed in subscrpted assignments of data frames'
ProcData$CROPDMGEXP <- as.character(ProcData$CROPDMG)
ProcData$PROPDMGEXP <- as.character(ProcData$PROPDMGEXP)

ProcData[(ProcData$PROPDMGEXP == "K" | ProcData$PROPDMGEXP == "k"), ]$PROPDMGEXP <- 3

ProcData[(ProcData$PROPDMGEXP == "M" | ProcData$PROPDMGEXP == "m"), ]$PROPDMGEXP <- 6

ProcData[(ProcData$PROPDMGEXP == "B" | ProcData$PROPDMGEXP == "b"), ]$PROPDMGEXP <- 9

ProcData[(ProcData$CROPDMGEXP == "K" | ProcData$CROPDMGEXP == "k"), ]$CROPDMGEXP <- 3

ProcData[(ProcData$CROPDMGEXP == "M" | ProcData$CROPDMGEXP == "m"), ]$CROPDMGEXP <- 6

ProcData[(ProcData$CROPDMGEXP == "B" | ProcData$CROPDMGEXP == "b"), ]$CROPDMGEXP <- 9
##-----------------------------------------------------------------------------------------------------------
##Compute combined economic damage (property damage + crops damage)

# multiply property and crops damage by 10 raised to the power of the exponent

suppressWarnings(ProcData$PROPDMG <- ProcData$PROPDMG * 10^as.numeric(ProcData$PROPDMGEXP)) #this action will give you NA's because there are not all numbers
suppressWarnings(ProcData$CROPDMG <- ProcData$CROPDMG * 10^as.numeric(ProcData$CROPDMGEXP))

# compute combined economic damage (property damage + crops damage)

suppressWarnings(ProcData$TOTAL_ECODMG <- ProcData$PROPDMG + ProcData$CROPDMG)


# delete 'PROPDMGEXP' and 'CROPDMGEXP'columns which have become unnecessary after conversion

ProcData <- ProcData[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG", "Names", "TOTAL_ECODMG")]

##--------------------------------------------------------------------------------------------------------------------------
## Important: In order to do the graphics check if the structure of your variables are correct (character, levels, etc.)
## Question 1: Across the US, which type of events are most harmful with respect to population health?

## Answer: order the number of injuries and fatalities from top to bottom and draw a picture of it

GroupFatalities <- aggregate(FATALITIES~Names,data=ProcData,FUN = sum) ##needs to be ordered
GroupFatalities <- GroupFatalities[order(GroupFatalities$FATALITIES,decreasing = TRUE),] #this is the syntax to sort the data
TopFatalities <- GroupFatalities[1:10,]
print(TopFatalities)

##Aggregate data for injuries
library(ggplot2)
GroupInjuries <- aggregate(INJURIES~Names,data=ProcData,FUN = sum)
GroupInjuries <- GroupInjuries[orders(GroupInjuries$INJURIES,decreasing = TRUE),]
TopInjuries <- GroupInjuries[1:10,]
print(TopInjuries)

##Agregate total economic damage

TotalEcoDmg <- aggregate(TOTAL_ECODMG~Names,data=ProcData,FUN = sum)
TotalEcoDmg <- TotalEcoDmg[order(TotalEcoDmg$TOTAL_ECODMG,decreasing = TRUE),]
TopEconDmg <- TotalEcoDmg[1:10,]
print(TopEconDmg)

#Graph
ggplot(TopEconDmg,aes(Names,TOTAL_ECODMG/(10^9))) + geom_bar(stat = "identity",color = "blue",fill = "lightblue") + coord_flip() + ggtitle("Top 10 Events with the Greatest Economic Damages") + labs(x="Events",y = "Economic Damage ($billlions)")



library(data.table)
library(R.utils)
library(knitr)
library(rmarkdown)
library(ggplot2)

## set working drive to maindir and create subdir (if it does not exist) to hold date
if (!file.exists("StormData.csv.bz2")) {
  fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileUrl, destfile="StormData.csv.bz2", method="curl")
}

storm <- fread(input = "StormData.csv.bz2")

## Check field names and data types
names(storm)
str(storm)

## Across the United States, which types of events are most harmful with respect to population health?

##Isolate columns needed for analysis
nms <- c("BGN_DATE","EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
subset <- storm[,..nms]

## Format date column, subset data for year >= 1996 (most complete dataset)
subset$BGN_DATE <- as.Date(subset$BGN_DATE, "%m/%d/%Y")
subset <- subset[,Year := year(subset$BGN_DATE)]
subset <- subset[Year >= 1996]

## Remove rows with zero property or crop damage, fatalities or loss
subset <- subset[FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 |CROPDMG > 0]

## Identify property and crop loss multipliers
table(subset$CROPDMGEXP)
table(subset$PROPDMGEXP)

## Calculate property and crop loss by multiplying DMG column by exponent. Sum PROP and CROP LOSS to get TOTAL LOSS
subset <- subset[PROPDMGEXP == "", PROPLOSS := PROPDMG]
subset <- subset[PROPDMGEXP == "K", PROPLOSS := PROPDMG*10^3]
subset <- subset[PROPDMGEXP == "M", PROPLOSS := PROPDMG*10^6]
subset <- subset[PROPDMGEXP == "B", PROPLOSS := PROPDMG*10^9]

subset <- subset[CROPDMGEXP == "", CROPLOSS := CROPDMG]
subset <- subset[CROPDMGEXP == "K", CROPLOSS := CROPDMG*10^3]
subset <- subset[CROPDMGEXP == "M", CROPLOSS := CROPDMG*10^6]
subset <- subset[CROPDMGEXP == "B", CROPLOSS := CROPDMG*10^9]

subset <- subset[,TOTALLOSS := PROPLOSS+CROPLOSS]
subset <- subset[,HUMANLOSS := FATALITIES+INJURIES]

subset$EVTYPE <- toupper(subset$EVTYPE)

human <- subset[,sum(HUMANLOSS), by = EVTYPE]
setnames(human, "V1", "HUMANLOSS")

human <- human[HUMANLOSS > quantile(HUMANLOSS, prob = 0.95)]
human <- human[order(-rank(HUMANLOSS))]

human$EVTYPE[(human$EVTYPE == "TSTM WIND")] <- "THUNDERSTORM WIND"
human$EVTYPE[(human$EVTYPE == "HURRICANE/TYPHOON")] <- "HURRICANE (TYPHOON)"

human <- human[,sum(HUMANLOSS), by = EVTYPE]
setnames(human, "V1", "HUMANLOSS")

## Repeat process for loss
econ <- subset[,sum(TOTALLOSS), by = EVTYPE]
setnames(econ, "V1", "TOTALLOSS")

econ <- econ[TOTALLOSS > quantile(TOTALLOSS, prob = 0.95)]
econ <- econ[order(-rank(TOTALLOSS))]

econ$EVTYPE[(econ$EVTYPE == "HURRICANE/TYPHOON")] <- "HURRICANE (TYPHOON)"
econ$EVTYPE[(econ$EVTYPE == "STORM SURGE")] <- "STORM SURGE/TIDE"


options(scipen = 999)
## Present Human Impact Results
g1 <- ggplot(human[1:9,], aes(x = reorder(EVTYPE, -HUMANLOSS), y = HUMANLOSS)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  xlab("Event Type") + ylab("Human Impact (Fatalities & Injuries)") +
  ggtitle ("Human Impact by Disaster Type")
g1

## Present Economic Impact Results
g2 <- ggplot(econ[1:10,], aes(x = reorder(EVTYPE, -TOTALLOSS), y = TOTALLOSS/(10^9))) +
  geom_bar(stat = "identity", fill = "darkgray") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  xlab("Event Type") + ylab("Economic Impact (billion USD)") +
  ggtitle ("Economic Impact by Disaster Type")
g2
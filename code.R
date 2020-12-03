library(data.table)
library(R.utils)
library(knitr)
library(rmarkdown)

## set working drive to maindir and create subdir (if it does not exist) to hold date
maindir <- getwd()
subdir <- "rawData"

if (file.exists(subdir)){
  setwd(file.path(maindir, subdir))
} else {
  dir.create(file.path(maindir, subdir))
  
}

## download and read raw data

if(!file.exists(paste0(subdir,"/StormData.csv.bz2"))){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile=paste0(subdir,"/stormData.csv.bz2"))
}

if(!file.exists(paste0(subdir,"/stormdata.csv")))
{
  storm <- fread(input = paste0(subdir,"/StormData.csv.bz2"))
}

## Check field names and data types
names(storm)
str(storm)

## Across the United States, which types of events are most harmful with respect to population health?

##Isolate columns needed for analysis
nms <- c("EVTYPE", "FATALITIES", "INJURIES", "INJURIES", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
q1_subset <- storm[,..nms]

fat <- q1_subset[,sum(FATALITIES), by = EVTYPE]
inj <- q1_subset[,sum(INJURIES), by = EVTYPE]
setnames(fat, "V1", "fatalities")
setnames(inj, "V1", "injuries")

## Arange both tables in decending order by fatalities and injuries
fat <- frank(fat, cols = "fatalities", order = -1)



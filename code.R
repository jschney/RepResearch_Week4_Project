library(data.table)
library(R.utils)
library(knitr)

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              "StormData.csv.bz2")

storm <- fread(input = "StormData.csv.bz2")
str(storm)


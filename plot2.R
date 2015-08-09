# ExData_Plotting1 assignment
library(data.table)
library(lubridate)
library(strptime)
library(dplyr)

# data url - Electric power consumption for one house over 3 years from the UC Irvine Machine Learning Repo
zipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

dataDir <- "./data/"
#dataSetDir <- paste(dataDir, "UCI HAR Dataset", sep="")

# prep: download file and store in data dir
# returns the scoped name of the zip file to use for extraction
downloadFile <- function(zipURL, dataDir) {
        if (!file.exists("data")) {
                dir.create("data")
        }
        zipFileName <- "Dataset.zip"
        # always download fresh set and capture data
        dateDownloaded <- format(Sys.time(), "%y-%m-%d.%H-%M-%S")
        fileNameResult <- paste(paste(dataDir, dateDownloaded, sep=""), zipFileName, sep="-")
        download.file(zipURL, destfile=fileNameResult, method="curl") 
        fileNameResult
}

# returns a data set reflecting the internal file path
# TODO: clean this up.. reads file but
getDataSetWithin <- function(zipFileName, path) {
        con <- unz(zipFileName, path)
        data <- data.table(
                read.table(
                        con, header=TRUE, sep=";", na.strings = "?", stringsAsFactors = FALSE))
}

# estimate data set size
dataSizeEstimate <- function(numRows) {
        sizeDate <- object.size(date())
        sizeTime <- object.size(Sys.time())
        sizeNumeric <- object.size(1.003)
        rowSize <- sizeDate + sizeTime + 7*sizeNumeric
        rowSize*numRows
}

# 0.) Load main data set
zipFileName <- downloadFile(zipURL, "./data/")

# 1.) pull out the file
# watch out - file may be too large.
# 133MB on Disk
# 2,075,259 rows and 9 columns: 1 Date (120Bytes) + 1 Time (312Bytes) + 7*numeric(48Bytes)
# size is approx 1593.798912  MB - so on an 8 GB machine we should be ok
# actual size of the data.frame was 258 MB..
householdPowerConsumption <- getDataSetWithin(zipFileName, "household_power_consumption.txt")

# 2.) filter to target dates
# note: I would like to do this filter on the input read with data.table's fread.. just don't know how
setkey(householdPowerConsumption, Date)

#mutate into Dates in order to make comparisons work.
householdPowerConsumption <- mutate(householdPowerConsumption, Date = dmy(Date)) %>% filter(Date %between% c("1/2/2007", "2/2/2007")) 

# 3.) Plot 2
#select(householdPowerConsumption, Global_active_power)
gap <- select(householdPowerConsumption,Global_active_power) %>% filter(!is.na(Global_active_power))

png(filename = "Plot2.png", width = 480, height = 480)
hist(gap$Global_active_power, 
     xlab="Global Active Power (kilowatts)", 
     ylab="Frequency",
     main="Global Active Power",
     col="red")

dev.off()

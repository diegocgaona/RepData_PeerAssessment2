require("dplyr")
## Set to english language and time
Sys.setlocale("LC_TIME", "English")
## Download and read the data
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipdatafile <- "./datafile.csv.bz2" ## create zipdata file
download.file(fileURL, destfile = zipdatafile, mode = "wb") ## download the file
rawdata <- read.csv(zipdatafile, header = TRUE, stringsAsFactors = FALSE)
head(rawdata)
## View(rawdata %>% group_by(EVTYPE) %>% summarise(n=n()))

data <- rawdata[c(2,7,8,23,24,25,26,27,28,37)]

## Replace the exponecials letters by numbers in the columns "PROPDMGEXP" and "CROPDMGEXP"
## Replace the letters by numbers
data$PROPDMGEXP = gsub("\\-|\\+|\\?|h|H|0","0",data$PROPDMGEXP)
data$PROPDMGEXP = gsub("k|K", "1000", data$PROPDMGEXP)
data$PROPDMGEXP = gsub("m|M", "1000000", data$PROPDMGEXP)
data$PROPDMGEXP = gsub("b|B", "1000000000", data$PROPDMGEXP)
data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)
data$PROPDMGEXP[is.na(data$PROPDMGEXP)] = 0

data$CROPDMGEXP <- as.character(data$CROPDMGEXP)
data$CROPDMGEXP = gsub("\\-|\\+|\\?|h|H|0","0",data$CROPDMGEXP)
data$CROPDMGEXP = gsub("k|K", "1000", data$CROPDMGEXP)
data$CROPDMGEXP = gsub("m|M", "1000000", data$CROPDMGEXP)
data$CROPDMGEXP = gsub("b|B", "1000000000", data$CROPDMGEXP)
data$CROPDMGEXP <- as.numeric(data$CROPDMGEXP)
data$CROPDMGEXP[is.na(data$CROPDMGEXP)] = 0

## Multiply the damages values by the exponents
data <- mutate(data, PROPDMG = PROPDMGEXP * PROPDMG) 
data <- mutate(data, CROPDMG = CROPDMGEXP * CROPDMG) 

colnames(data) <- c("begin_date","state","event_type","fatalities","injuries",
                    "property_damage","property_damage_exp","crop_damage","crop_damage_exp","reference_number")

## Transform the date variables in Posix format
data$begin_date <- as.Date(strptime(data$begin_date, format = "%m/%d/%Y %H:%M:%S"))  
data$begin_year <- as.factor(format(data$begin_date,format="%Y"))

## View(data %>% group_by(state,event_type) %>% summarise(n=n()))
## SUbset to only use the util data for this analysis
subdata <- data[c(1,3,4,5,6,8,11)]
## Convert all the event_types to factor and uppercase to reduce the mistyped errors
subdata$event_type <- factor(toupper(as.character(subdata$event_type)))


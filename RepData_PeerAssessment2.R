require("dplyr")
require("tidyr")
require("ggplot2")
require("reshape2")
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
data$begin_date <- as.factor(format(data$begin_date,format="%Y")) ## I will use only the year

## View(data %>% group_by(state,event_type) %>% summarise(n=n()))
## For this analysis, i will not use all the columns (i will mantain them only for future analysis)
## Now i will Subset to only use the util data for this analysis
subdata <- data[c(1,3,4,5,6,8)]

## Convert all the event_types to factor and uppercase to reduce the mistyped errors
subdata$event_type <- factor(toupper(as.character(subdata$event_type)))

# Now i subset for only the data with fatalities, injuries, property or crop damage.
subdata <- subset(subdata, fatalities > 0 | injuries > 0 | property_damage > 0 | crop_damage > 0,
                  select=begin_date:crop_damage)

## I investigate the evtypes by searching some event type words found in Storm Data Event Table.
## How many variables contain these words, which of them i can categorize as the same?
## teste <- subdata[grep("AVALAN", subdata$event_type),]
## teste <- as.data.frame(as.character(unique(teste$event_type)))
## View(teste)

## Replaces: Ex. Replace all that contains "FLOOD" (FLASH FLOOD and others) by exactly "FLOOD" 
## The order is important, because the replacement is sequencial. Example, some events have: 
## "WIND" and "THUNDERSTORM", i prioritized "THUNDERSTORM" before wind.
subdata$event_type[grepl("TORNADO", subdata$event_type)] <- "TORNADO"
subdata$event_type[grepl("HURRICANE", subdata$event_type)] <- "HURRICANE"
subdata$event_type[grepl("FLOOD", subdata$event_type)] <- "FLOOD"
subdata$event_type[grepl("BLIZZARD", subdata$event_type)] <- "BLIZZARD"
subdata$event_type[grepl("DROUGHT", subdata$event_type)] <- "DROUGHT"
subdata$event_type[grepl("HEAT", subdata$event_type)] <- "HEAT"
subdata$event_type[grepl("AVALANC", subdata$event_type)] <- "AVALANCHE"
subdata$event_type[grepl("COLD", subdata$event_type)] <- "COLD"
subdata$event_type[grepl("THUN.*.ORM|TSTM|THUN.*.TROM", subdata$event_type)] <- "THUNDERSTORM"
subdata$event_type[grepl("TROPICAL STORM", subdata$event_type)] <- "TROPICAL STORM"
subdata$event_type[grepl("SNOW", subdata$event_type)] <- "SNOW"
subdata$event_type[grepl("WIND", subdata$event_type)] <- "WIND"
subdata$event_type[grepl("HAIL", subdata$event_type)] <- "HAIL"
subdata$event_type[grepl("LIGHTNING", subdata$event_type)] <- "LIGHTNING"
subdata$event_type[grepl("RAIN", subdata$event_type)] <- "RAIN"
subdata$event_type[grepl("FROST|FREEZE", subdata$event_type)] <- "FROST/FREEZE"
subdata$event_type = gsub("WILDFIRE|WILDFIRES|WILD///FOREST FIRE|WILD FIRES", "FIRE", subdata$event_type)
## Create a column with the sum of fatalities and injuries
summhealth <- mutate(subdata, health = fatalities + injuries)
## Subset the data to show only the events harmful to health and summarize the data by event type.
summhealth <- aggregate(cbind(fatalities, injuries, health) ~ event_type, summhealth[which(summhealth$health != 0),],
                        sum, na.action = na.pass)
## Arrange to descend by the more harmful events
summhealth <- arrange(summhealth, desc(health))

## Create a column with the sum of property_damage and crop_damage
summeconon <- mutate(subdata, economic = property_damage + crop_damage)
## Subset the data to show only the events with economic consequences and summarize the data by event type.
summeconon <- aggregate(cbind(property_damage, crop_damage, economic) ~ event_type, summeconon[which(summeconon$economic != 0),],
                        sum, na.action = na.pass)
summeconon <- arrange(summeconon, desc(economic))

## Make a general summary only to explore the data
summ <- mutate(subdata, importance = fatalities + injuries + property_damage + crop_damage)
summ <- aggregate(cbind(fatalities, injuries, property_damage, crop_damage, importance) ~ 
                        event_type, summ[which(summ$importance != 0),],
                        sum, na.action = na.pass)
summ <- arrange(summ, desc(importance))

teste <- melt(summhealth,id=c("event_type"),measure.vars= c("fatalities", "injuries","health"))
teste <- summhealth
teste <- cbind(teste, summhealth$health)
colnames(teste) <- c("event_type", "variable", "value", "health")
teste <- group_by(teste,event_type)
teste <- arrange(teste,desc(fatalities))

p2 <- ggplot(summhealth[c(1:10), c(1:4)], aes(x = event_type, y = health, fill = -health)) + 
      geom_bar(stat = "identity") +
      scale_fill_gradient("Health damage") +
      xlab("Weather event type") + ylab("Health damage") +
      ggtitle ("Top 10 weather events types causing health harmful") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold")) +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      theme(axis.title.x = element_text(lineheight=.8, face="bold")) +
      theme(axis.title.y = element_text(lineheight=.8, face="bold"))            
p2



p2 <- ggplot(teste[c(1:10), c(1:3)], reorder(event_type, value), aes(x = event_type, y = health, fill = variable)) + 
      geom_bar(stat = "identity") +
      facet_grid(variable ~., ncol = 1)
p2
#scale_fill_gradient("Health damage") +
xlab("Weather event type") + ylab("Health damage") +
      ggtitle ("Top 10 weather events types causing health harmful") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold")) +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      theme(axis.title.x = element_text(lineheight=.8, face="bold")) +
      theme(axis.title.y = element_text(lineheight=.8, face="bold")) #+
#+
#scale_x_discrete(breaks = seq(1960, 2050, by=2)) +


---
title: "NOAA Storm Database analysis for finding severe weather events. "
output:
  html_document:
    keep_md: false
---
##Synopsis
The Goal of this study is to explore the NOAA Storm Database and answer some questions about severe weather events in USA. We want to find out  answers about which events  are most harmful to population health and which events are most damaging to Economy.

Analyzing we find that flood ,Hurricaine, tornado, storm are among the events that cause the most damage to property and Flood 2 to 3 times more damaging than any other event types.

Tornado m Wind and heat has are among the  events that case most death and injury for people and most harmful for population health. Tornado is10 times more dangerous than any other event types.

##Data Processing
###Load Data

```r
library(RCurl)
```

```
## Loading required package: bitops
```

```r
library(R.utils)
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following object is masked from 'package:RCurl':
## 
##     clone
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.34.0 (2014-10-07) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:RCurl':
## 
##     reset
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipFile <-'repdata-data-StormData.csv.bz2'
fileName<-'repdata-data-StormData.csv'
#bin <- getBinaryURL(fileUrl,ssl.verifypeer=FALSE)
#con <- file(zipFile, open = "wb")
#writeBin(bin, con)
#close(con)
#bunzip2("repdata-data-StormData.csv.bz2", overwrite=T, remove=F)
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y"))  
```

```
## in method for 'coerce' with signature '"character","myDate"': no definition for class "myDate"
```

```r
data <- read.csv(fileName, header=TRUE,
                 na.strings="NA",colClasses=c("BGN_DATE"="myDate")) 
if ( nrow(data) != 902297) {
    print("Data does not have 902297 rows as expected..Check data source!")
}else {
    print ("Data looks OK!")
}
```

```
## [1] "Data looks OK!"
```

```r
colnames<-colnames(data)
print(colnames)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```
### Finding  meanful Subset of Data for analysis.

```r
library(dplyr)
library(lubridate)
findPropDamage<-function(prop,propexp) {
    inprop =as.numeric(prop)
    inpropexp=toupper(as.character(propexp))
    out =inprop
    out= ifelse( is.na(propexp),inprop,
         ifelse(inpropexp == "B",inprop*10^9,
         ifelse(inpropexp == "M",inprop*10^6,
         ifelse(inpropexp == "K",inprop*10^3,
         ifelse(inpropexp == "H",inprop*10^2,
         inprop)                  
    ))))
    return(out)
}
findHealthDamage<-function(fatalities,injuries) {
    return(ifelse(is.na(fatalities),0,as.numeric(fatalities)) +
               ifelse(is.na(injuries),0,as.numeric(injuries)))
}

#select appropriate columns
storm_data<-
    data %>%
    mutate(EVT_YEAR=year(BGN_DATE)) %>%
    mutate(PropertyDamage = findPropDamage(PROPDMG,PROPDMGEXP)) %>%
    mutate(HumanAndHealthCost = findHealthDamage(FATALITIES,INJURIES)) %>%
    select(EVT_YEAR,EVTYPE,PropertyDamage,HumanAndHealthCost,PROPDMG,PROPDMGEXP,FATALITIES,INJURIES)
summary(storm_data$ProppertyDamage)
```

```
## Length  Class   Mode 
##      0   NULL   NULL
```

```r
#find top 25 event that cause most damage to property

storm_data_top25_p<-
    storm_data %>%
    group_by(EVTYPE) %>%
    summarise(TotalDamage= sum(PropertyDamage)) %>%
    arrange(desc(TotalDamage)) %>%
    head(25)

storm_data_top25_h<-
    storm_data %>%
    group_by(EVTYPE) %>%
    summarise(TotalHealthCost= sum(HumanAndHealthCost)) %>%
    arrange(desc(TotalHealthCost)) %>%
    head(25)
write.table(storm_data_top25_p)
```

```
## "EVTYPE" "TotalDamage"
## "1" "FLOOD" 144657709807
## "2" "HURRICANE/TYPHOON" 69305840000
## "3" "TORNADO" 56937160778.7
## "4" "STORM SURGE" 43323536000
## "5" "FLASH FLOOD" 16140812067.1
## "6" "HAIL" 15732267542.7
## "7" "HURRICANE" 11868319010
## "8" "TROPICAL STORM" 7703890550
## "9" "WINTER STORM" 6688497251
## "10" "HIGH WIND" 5270046295
## "11" "RIVER FLOOD" 5118945500
## "12" "WILDFIRE" 4765114000
## "13" "STORM SURGE/TIDE" 4641188000
## "14" "TSTM WIND" 4484928495
## "15" "ICE STORM" 3944927860
## "16" "THUNDERSTORM WIND" 3483121284
## "17" "HURRICANE OPAL" 3172846000
## "18" "WILD/FOREST FIRE" 3001829500
## "19" "HEAVY RAIN/SEVERE WEATHER" 2.5e+09
## "20" "THUNDERSTORM WINDS" 1735961003.2
## "21" "TORNADOES, TSTM WIND, HAIL" 1.6e+09
## "22" "SEVERE THUNDERSTORM" 1205360000
## "23" "DROUGHT" 1046106000
## "24" "HEAVY SNOW" 932589141.7
## "25" "LIGHTNING" 928659446.7
```

```r
write.table(storm_data_top25_h)
```

```
## "EVTYPE" "TotalHealthCost"
## "1" "TORNADO" 96979
## "2" "EXCESSIVE HEAT" 8428
## "3" "TSTM WIND" 7461
## "4" "FLOOD" 7259
## "5" "LIGHTNING" 6046
## "6" "HEAT" 3037
## "7" "FLASH FLOOD" 2755
## "8" "ICE STORM" 2064
## "9" "THUNDERSTORM WIND" 1621
## "10" "WINTER STORM" 1527
## "11" "HIGH WIND" 1385
## "12" "HAIL" 1376
## "13" "HURRICANE/TYPHOON" 1339
## "14" "HEAVY SNOW" 1148
## "15" "WILDFIRE" 986
## "16" "THUNDERSTORM WINDS" 972
## "17" "BLIZZARD" 906
## "18" "FOG" 796
## "19" "RIP CURRENT" 600
## "20" "WILD/FOREST FIRE" 557
## "21" "RIP CURRENTS" 501
## "22" "HEAT WAVE" 481
## "23" "DUST STORM" 462
## "24" "WINTER WEATHER" 431
## "25" "TROPICAL STORM" 398
```
### select Event groups

```r
# Based on the above list of top 25 event we catagorize the events into groups that are similar and show up in the top 25 list



eventType <- function(type) {
    out="OTHER"
    out =
        ifelse(grepl("WIND",toupper(storm_data$EVTYPE)), "Wind",
        ifelse(grepl("STORM",toupper(storm_data$EVTYPE)), "Storm",
        ifelse(grepl("TSM",toupper(storm_data$EVTYPE)), "Storm",
        ifelse(grepl("FLOOD",toupper(storm_data$EVTYPE)), "Flood",
        ifelse(grepl("RAIN",toupper(storm_data$EVTYPE)), "Rain",
        ifelse(grepl("HURRICANE",toupper(storm_data$EVTYPE)), "Hurricane",
        ifelse(grepl("HAIL",toupper(storm_data$EVTYPE)), "Hail",
        ifelse(grepl("TORNADO",toupper(storm_data$EVTYPE)), "Tornado",
        ifelse(grepl("HEAT",toupper(storm_data$EVTYPE)), "Heat",
        ifelse(grepl("FIRE",toupper(storm_data$EVTYPE)), "Fire","Others")
    )))))))))
    return(out)
}
storm_data<-
    storm_data %>%
    mutate(EVTYPE_E=eventType(EVTYPE))
```
##Results
### Property Damage & Health Impact ( Loss of life and injuries) from various events 

```r
library(ggplot2)
library(grid)
library(gridExtra)
storm_data_p <-
  storm_data %>%
    group_by(EVT_YEAR,EVTYPE_E) %>%
      summarize(TotalPropDamage=sum(PropertyDamage))

plot1<-ggplot(storm_data_p,aes(x=EVT_YEAR,y=TotalPropDamage,fill=factor(EVTYPE_E))) +
    geom_bar(stat="identity") + 
    labs(y="Total Property Damage",x= "year")

storm_data_pe <-
  storm_data %>%
  group_by(EVTYPE_E) %>%
  summarize(TotalPropDamage=sum(PropertyDamage)) %>%
  arrange(desc(TotalPropDamage)) 

storm_data_pe$EVTYPE_E <- factor(storm_data_pe$EVTYPE_E, levels=unique(as.character(storm_data_pe$EVTYPE_E)) )

plot2<-ggplot(storm_data_pe,aes(x=EVTYPE_E,y=TotalPropDamage)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle=90, vjust=1)) +
    labs(y="Total Property Damage",x="")
    

require(gridExtra)
grid.arrange(plot1, plot2, nrow=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
storm_data_h <-
  storm_data %>%
    group_by(EVT_YEAR,EVTYPE_E) %>%
      summarize(TotalHealthCost=sum(HumanAndHealthCost))

plot1<-ggplot(storm_data_h,aes(x=EVT_YEAR,y=TotalHealthCost,fill=factor(EVTYPE_E))) +
    geom_bar(stat="identity") + 
    labs(y="#Death and Injuries",x= "Year")


storm_data_he <-
  storm_data %>%
  group_by(EVTYPE_E) %>%
  summarize(TotalHealthCost=sum(HumanAndHealthCost)) %>%
  arrange(desc(TotalHealthCost)) 

storm_data_he$EVTYPE_E <- factor(storm_data_he$EVTYPE_E, levels=unique(as.character(storm_data_he$EVTYPE_E)) )

plot2<-ggplot(data=storm_data_he,aes(x=EVTYPE_E,y=TotalHealthCost)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle=90, vjust=1)) +
    labs(y="#Death and Injuries",x="")
require(gridExtra)
grid.arrange(plot1, plot2, nrow=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

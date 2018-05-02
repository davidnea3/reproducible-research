---
title: Analysis of impact of weather events on public health and economic damage in
  the US
author: "Davyd"
date: "May, 2018"
output:
  html_document:
    keep_md: yes
---

## Synopsis 

Based on the data from U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, this paper explores the effects of severe natural events from 1950 to November 2011, more specifically the most harmful events with regard to human healths and economic damages. The data was read and cleaned up then got aggregated by event types. Harmful events to humans are measured by the number of fatalities and injuries while detrimental events to economics are measured by the number of property and crop damages. It turned out that tornado has the most negative impact causing highest fatality and injury, while flood led to the most property damage and drought led to the most crop damage.

## Questions Addressed

1. Across the United States, which types of events (as indicated in the ???????????????????????? variable) are most harmful with respect to population health?

1. Across the United States, which types of events have the greatest economic consequences?

## Data Processing




```r
rawData = read.csv('repdata_data_StormData.csv', stringsAsFactors=FALSE)   
names(rawData)
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

```r
dim(rawData)
```

```
## [1] 902297     37
```

Among all these dimensions, we will only select 8 variables that are related to this analysis, which are:

* STATE
* EVTYPE
* FATALITIES
* INJURIES
* PROPDMG
* PROPDMGEXP
* CROPDMG
* CROPDMGEXP



```r
variName<- c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP","CROPDMG", "CROPDMGEXP")
data <-rawData[variName]
dim(data)
```

```
## [1] 902297      8
```

```r
head(data)
```

```
##   STATE  EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1    AL TORNADO          0       15    25.0          K       0           
## 2    AL TORNADO          0        0     2.5          K       0           
## 3    AL TORNADO          0        2    25.0          K       0           
## 4    AL TORNADO          0        2     2.5          K       0           
## 5    AL TORNADO          0        2     2.5          K       0           
## 6    AL TORNADO          0        6     2.5          K       0
```

Look at missing values 

```r
sum(is.na(data))
```

```
## [1] 0
```

Here we calculate the type of events that would cause large number of fatalities and injuries from 1950 to November 2011. We aggregate the data using sum first then rank by decreasing order.

```r
fatal <- aggregate(FATALITIES ~ EVTYPE, data = data, FUN = sum)
injury <- aggregate(INJURIES ~ EVTYPE, data = data, FUN = sum)
fatalOrder <- fatal[order(fatal$FATALITIES, decreasing = TRUE), ]
injuryOrder <- injury[order(injury$INJURIES, decreasing = TRUE), ]
```

Let's look at property damage and crop damage unique data first.

```r
unique(data$PROPDMGEXP)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```

```r
unique(data$CROPDMGEXP)
```

```
## [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
```

We can see that there are both numerical and alphbetical characters to represent significant digits. For example, "8" would be 10^8 and "K" or "k" would be thousands. So we want to make the number format consistent first.


```r
symbol <- c("","+","-","?",0:9,"h","H","k","K","m","M","b","B")
factor <- c(rep(0,4),0:9,2,2,3,3,6,6,9,9)
multipler <-data.frame(symbol,factor)
```



Here we calculate the type of events that would cause large number of property and crop damage from 1950 to November 2011. We aggregate the data using sum first then rank by decreasing order.

```r
data$PROPDMGVAL <-data$PROPDMG* 10^multipler[match(data$PROPDMGEXP,multipler$symbol),2]
data$CROPDMGVAL <-data$CROPDMG* 10^multipler[match(data$CROPDMGEXP,multipler$symbol),2]

propdmg <- aggregate(PROPDMGVAL ~EVTYPE,data=data, FUN = sum)
cropdmg <- aggregate(CROPDMGVAL ~EVTYPE,data=data, FUN= sum)

propOrder <-propdmg[order(propdmg$PROPDMGVAL,decreasing=TRUE),]
cropOrder <-cropdmg[order(cropdmg$CROPDMGVAL,decreasing=TRUE),]
```


## Results

**Harmful events to human healths: fatalities & injuries**

Now we draw two figures with top 10 fatality and injury event types separately. 

```r
par(mfrow=c(1,2), mar= c(10,4,1,1), mgp=c(3,1,0), cex=0.8)
barplot(fatalOrder[1:10,2], col = rainbow(10), legend.text=fatalOrder[1:10,1],ylim=c(0,6000),ylab="Fatality", main="Top 10 events causing highest fatality")
barplot(injuryOrder[1:10,2], col = rainbow(10), legend.text=injuryOrder[1:10,1],ylim=c(0,18000),ylab="Injury", main="Top 10 events causing highest injury")
```

![](PA2_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The plot showed that Tornado and Flood are among top 3 reasons for both highest number of fatalities and injuries.

**Harmful events to economic damages: property & crop damage**

Now we draw two figures with top 10 property and crop damage types separately. 

```r
par(mfrow=c(1,2), mar= c(10,4,1,1), mgp=c(3,1,0), cex=0.8)
barplot(propOrder[1:10,2]/10^9, col = rainbow(10), legend.text=propOrder[1:10,1],ylim=c(0,160),ylab="Property Damage", main="Top 10 events causing highest property damage")
barplot(cropOrder[1:10,2]/10^9, col = rainbow(10), legend.text=cropOrder[1:10,1],ylim=c(0,15),ylab="Crop Damage", main="Top 10 events causing highest crop damage")
```

![](PA2_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The plot showed that flood, hirricane/typhoon and tornado hit the properties the hardest while drought and different types of flood hit the crop the hardest.



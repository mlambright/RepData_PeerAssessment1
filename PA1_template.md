---
title: "Reproducible Research Assignment 1"
output: html_document
---

***

Part 1: Loading and preprocessing the data
------------------------------------------
  1. Read in the data from the *activity.csv* file located in the working directory. The zip file containing *activity.csv* can be downloaded [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

```r
rawData <- read.csv('activity.csv', stringsAsFactors=FALSE)
rawData$steps <- as.numeric(rawData$steps)
head(rawData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

  2. Generate a data frame with medians for steps from each 5-minute interval

```r
medianTable <- data.frame(interval = unique(rawData$interval), median = as.numeric(NA))
for (i in 1:nrow(medianTable)) {
  medianTable$median[i] <- median(rawData[rawData$interval==medianTable$interval[i], 1], na.rm=TRUE)
}
summary(medianTable)
```

```
##     interval        median     
##  Min.   :   0   Min.   : 0.00  
##  1st Qu.: 589   1st Qu.: 0.00  
##  Median :1178   Median : 0.00  
##  Mean   :1178   Mean   : 3.96  
##  3rd Qu.:1766   3rd Qu.: 0.00  
##  Max.   :2355   Max.   :60.00
```

  3. Replace missing values in the steps column with corresponding 5-minute interval medians from *medianTable*, format to match *rawData*

```r
cleanedData <- merge(rawData, medianTable, all.x=TRUE)
cleanedData$steps[is.na(cleanedData$steps)] <- cleanedData$median[is.na(cleanedData$steps)]
cleanedData <- cleanedData[order(cleanedData$date, cleanedData$interval),c(2,3,1)]
row.names(cleanedData) <- 1:nrow(cleanedData)
head(cleanedData)
```

```
##   steps       date interval
## 1     0 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     0 2012-10-01       25
```

***

Part 2: Mean total steps per day
--------------------------------
  1. Create a data frame with dates and total steps

```r
dayData <- data.frame(date = unique(cleanedData$date), steps = as.numeric(NA))
for (i in 1:nrow(dayData)) {
  dayData$steps[i] <- sum(cleanedData$steps[cleanedData$date == dayData$date[i]])
}
head(dayData)
```

```
##         date steps
## 1 2012-10-01  1141
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

  2. Plot a histogram of total steps per day

```r
hist(as.numeric(dayData$steps), xlab='Total daily steps', main='Daily step totals')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

  3. Find mean and median total steps per day

```r
mean(dayData$steps)
```

```
## [1] 9504
```

```r
median(dayData$steps)
```

```
## [1] 10395
```

***

Part 3: Average daily activity pattern
--------------------------------------
  1. Create a data frame with 5 minute intervals and average total steps

```r
timeData <- data.frame(interval = unique(cleanedData$interval), steps = as.numeric(NA))
for (i in 1:nrow(timeData)) {
  timeData$steps[i] <- mean(cleanedData$steps[cleanedData$interval == timeData$interval[i]])
}
```

  2. Create time-series plot of total steps by 5-minute interval

```r
plot(timeData$interval, timeData$steps, xlab = 'Interval', ylab = 'Average total steps', 
     main = 'Average total steps by 5-minute interval', type='l')
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

  3. Identify which 5-minute interval corresponds to the highest average steps

```r
timeData[timeData$steps == max(timeData$steps),]
```

```
##     interval steps
## 104      835 181.6
```

***

Part 4: Differences between weekdays and weekends
-------------------------------------------------
  1. Create a new factor variable to separate 'weekday's from 'weekend's

```r
splitVec <- c('weekday', 'weekend')
cleanedData$split <- as.factor(splitVec[(strptime(cleanedData$date, '%Y-%m-%d')$wday %in% c(6,7)) + 1])
head(cleanedData)
```

```
##   steps       date interval   split
## 1     0 2012-10-01        0 weekday
## 2     0 2012-10-01        5 weekday
## 3     0 2012-10-01       10 weekday
## 4     0 2012-10-01       15 weekday
## 5     0 2012-10-01       20 weekday
## 6     0 2012-10-01       25 weekday
```

  2. Average steps for each 5 minute intervals across weekends and weekdays

```r
timeSplitData <- cleanedData[,c(3, 4)]
timeSplitData <- unique(timeSplitData)
timeSplitData$steps <- as.numeric(NA)
for (i in 1:nrow(timeSplitData)) {
  timeSplitData$steps[i] <- mean(cleanedData$steps[cleanedData$interval == timeSplitData$interval[i] &
                                                     cleanedData$split == timeSplitData$split[i]])
}
```

  3. Make a panel plot comparing weekdends to weekdays in average steps for 5-minute intervals

```r
library(lattice)
xyplot(steps ~ interval | split, timeSplitData, type = "l", layout = c(1,2))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

---
title: "Reproducible Research: Peer Assignment 1""
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
library(plyr)
library(reshape2)

data <- read.csv("activity.csv",stringsAsFactors=FALSE)
```

## What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day, ignoring missing values in the dataset.


```r
melted <- melt(data,c("date"),na.rm=TRUE)
stepsByDay <- dcast(melted,date~variable,sum)
```


```r
hist(stepsByDay[,2], xlab="sum of steps taken per day",main="")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Calculate the mean and median total number of steps taken per day.

```r
mean(stepsByDay[,2])
```

```
## [1] 9354
```

```r
median(stepsByDay[,2])
```

```
## [1] 10395
```


## What is the average daily activity pattern?
Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
melted <- melt(data,c("interval"), measure=c("steps"),na.rm=TRUE)
stepsByInterval <- dcast(melted,interval~variable,mean)

plot(stepsByInterval,type="l",xlab="5-minute interval",ylab="average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsByInterval[stepsByInterval$steps == max(stepsByInterval$steps),]
```

```
##     interval steps
## 104      835 206.2
```

## Inputing missing values
Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

To fill in missing values, replace NA values for given intervals with their corresponding daily steps average. For example, if time interval 1 is an NA value, replace with the calculated daily average for time interval 1.


```r
data_na <- data[!complete.cases(data),]

data_na_filled <- merge(data_na[,c("date","interval")],stepsByInterval,by="interval")

data_na_filled <- data_na_filled[,c("steps","date","interval")]

data_filled <- rbind(data_na_filled, data[complete.cases(data),])
```


###Make a histogram of the total number of steps taken each day, and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
melted <- melt(data_filled,c("date"),na.rm=TRUE)
stepsByDay <- dcast(melted,date~variable,sum)

hist(stepsByDay[,2],xlab="sum of steps taken per day",main="")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


```r
mean(stepsByDay[,2])
```

```
## [1] 10766
```

```r
median(stepsByDay[,2])
```

```
## [1] 10766
```

The values from the estimates from the first part of the assignment vary slightly. The impact of inputing missing data makes the total steps distribution appear more normal.

## Are there differences in activity patterns between weekdays and weekends?

```r
data_filled$date <- as.Date(data_filled$date,"%Y-%m-%d")
```

Assign weekday and weekend values in new day column.

```r
for (i in 1:length(data_filled$date)) {
        noDayofWeek <- as.POSIXlt(data_filled$date[i])$wday
        
        if (noDayofWeek > 0 & noDayofWeek < 6) {
                data_filled$day[i] <- c("weekday")
        }
        else {
                data_filled$day[i] <- c("weekend")
        }
}
```

Plot using lattice:

```r
melted <- melt(data_filled,c("interval","day"), measure=c("steps"),na.rm=TRUE)
stepsByInterval <- dcast(melted,interval + day~variable,mean)

library(lattice)
xyplot(steps~interval | day, data=stepsByInterval, layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

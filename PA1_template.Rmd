# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
```{r, echo=TRUE}
library(plyr)
library(reshape2)

data <- read.csv("activity.csv",stringsAsFactors=FALSE)
```

## What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day, ignoring missing values in the dataset.

```{r, echo=TRUE}
melted <- melt(data,c("date"),na.rm=TRUE)
stepsByDay <- dcast(melted,date~variable,sum)

hist(stepsByDay[,2], xlab="sum of steps taken per day",main="")

```

Calculate the mean and median total number of steps taken per day.
```{r, echo=TRUE}
mean(stepsByDay[,2])
median(stepsByDay[,2])
```


## What is the average daily activity pattern?
Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r, echo=TRUE}
melted <- melt(data,c("interval"), measure=c("steps"),na.rm=TRUE)
stepsByInterval <- dcast(melted,interval~variable,mean)

plot(stepsByInterval,type="l",xlab="5-minute interval",ylab="average number of steps")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo=TRUE}
stepsByInterval[stepsByInterval$steps == max(stepsByInterval$steps),]
```

## Imputing missing values
Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r,echo=TRUE}
sum(!complete.cases(data))
```

To fill in missing values, replace NA values for given intervals with their corresponding daily steps average.

```{r,echo=TRUE}
data_na <- data[!complete.cases(data),]

data_na_filled <- merge(data_na[,c("date","interval")],stepsByInterval,by="interval")

data_na_filled <- data_na_filled[,c("steps","date","interval")]

data_filled <- rbind(data_na_filled, data[complete.cases(data),])
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, echo=TRUE}
melted <- melt(data_filled,c("date"),na.rm=TRUE)
stepsByDay <- dcast(melted,date~variable,sum)

hist(stepsByDay[,2],xlab="sum of steps taken per day",main="")

mean(stepsByDay[,2])
median(stepsByDay[,2])
```

The values from the estimates from the first part of the assignment vary slightly. The impact of inputing missing data makes the total steps distribution appear more normal.

## Are there differences in activity patterns between weekdays and weekends?
```{r,echo=TRUE}
data_filled$date <- as.Date(data_filled$date,"%Y-%m-%d")
```

Assign weekday and weekend values in new day column.
```{r,echo=TRUE}
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
```{r,echo=TRUE}
melted <- melt(data_filled,c("interval","day"), measure=c("steps"),na.rm=TRUE)
stepsByInterval <- dcast(melted,interval + day~variable,mean)

library(lattice)
xyplot(steps~interval | day, data=stepsByInterval, layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps")
```

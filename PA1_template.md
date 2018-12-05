---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
# Load packages
library(dplyr)
```

## Loading and preprocessing the data

```r
if (!file.exists("activity.csv")){
  unzip("./activity.zip")
}
data = read.csv("activity.csv", colClasses = c("numeric","Date","numeric"))

# We can omit all entries with NA but this will remove the intervals completely 
# data = na.omit(rawdata)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
stepsTaken = tapply(data$steps, data$date, function(x)sum(x,na.rm = T))
```

2. Make a histogram of the total number of steps taken each day


```r
hist(stepsTaken, 
     xlab="Total number of steps taken each day", 
     ylab="Count", 
     main="Histogram of total number of steps taken each day",
     col=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meansteps = mean(stepsTaken)
mediansteps = median(stepsTaken)
```

Average number of total steps taken is: 9354.2295082
Median of total steps taken is: 1.0395\times 10^{4}

## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalMean<- data %>%
    group_by(interval) %>%
    summarize(stepsMean=mean(steps, na.rm = T))

plot(intervalMean$interval, intervalMean$stepsMean, 
     type="l",
     xlab="Interval",
     ylab="Average steps taken",
     main="Average steps taken during 5 minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval = intervalMean$interval[which.max(intervalMean$stepsMean)]
```

The highest average step count happened during interval 835


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
NAs = is.na(data$steps)
naNum = sum(NAs)
```

Number of missing values in dataset 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataClean = data

for (i in 1:nrow(dataClean)){
  if (is.na(dataClean$steps[i])){
    idx = which(intervalMean$interval == dataClean$interval[i])
    dataClean$steps[i]=intervalMean$stepsMean[idx]
  
  }
  
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsTakenCleaned = tapply(dataClean$steps, dataClean$date, function(x)sum(x))

hist(stepsTakenCleaned, 
     xlab="Total number of steps taken each day", 
     ylab="Count", 
     main="Histogram of total number of steps taken each day",
     col=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
meanstepsCleaned = mean(stepsTakenCleaned)
medianstepsCleaned = median(stepsTakenCleaned)
```

Average number of total steps taken is: 1.0766189\times 10^{4}
Median of total steps taken is: 1.0766189\times 10^{4}

The median and mean both have increased.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
dataClean$wDay <- factor((weekdays(dataClean$date) %in% weekdays1), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
intervalMean2<- dataClean %>%
  group_by(wDay,interval) %>%
  summarize(stepsMean=mean(steps, na.rm = T))
```


```r
xyplot(stepsMean ~ interval | wDay, data = intervalMean2, type='l', layout = c(1,2),
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

There are more activities earlier in the day during weekdays.

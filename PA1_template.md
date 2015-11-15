# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### Load the data

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activity <- read.csv("C:/Development/Git Repos/RepData_PeerAssessment1/activity.csv")
activityClean <- activity[complete.cases(activity), ]
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day.

```r
activityPerDay <- aggregate(x = activityClean$steps, by = list(activityClean$date), FUN = sum, na.rm = TRUE)
```

### Make a histogram of the total number of steps taken each day.

```r
colnames(activityPerDay) = c("date", "steps")
hist(activityPerDay$steps, main = "Total Number of Steps/day", xlab = "Steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

### Mean of the total number of steps taken per day

```r
mean(activityPerDay$steps)
```

```
## [1] 10766.19
```

### Median of the total number of steps taken per day

```r
median(activityPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

### Group the data first.

```r
avgActivityPerInterval <- aggregate(x = activityClean$steps, by = list(activityClean$interval), FUN = mean, na.rm = TRUE)
```

### Make the plot.

```r
colnames(avgActivityPerInterval) = c("interval", "steps")
plot(x = avgActivityPerInterval$interval, y = avgActivityPerInterval$steps, type = "l", xlab = "Interval", ylab = "Avg. Number of Steps", main = "Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgActivityPerInterval[which.max(avgActivityPerInterval$steps), "interval"]
```

```
## [1] 835
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity[, ]))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
### I opted to use the means for each of the 5-minute intervals.

```r
activityCompleted <- activity
for (i in 1:nrow(activityCompleted)) {
	if (is.na(activityCompleted[i, "steps"])) {
		activityCompleted[i, "steps"] <- avgActivityPerInterval[avgActivityPerInterval$interval 
													   == activityCompleted[i, "interval"], "steps"]
	}
}
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityCompletedPerDay <- aggregate(x = activityCompleted$steps, by = list(activityCompleted$date), FUN = sum)
```

### Make a histogram of the total number of steps taken each day, calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
colnames(activityCompletedPerDay) = c("date", "steps")
hist(activityCompletedPerDay$steps, main = "Total Number of Steps/day (imputed dataset)", xlab = "Steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
mean(activityCompletedPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(activityCompletedPerDay$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityCompleted$daytype <- ""
activityCompleted$daytype[weekdays(as.Date(activityCompleted$date)) %in% c("Saturday", "Sunday")] <- "weekend"
activityCompleted$daytype[!weekdays(as.Date(activityCompleted$date)) %in% c("Saturday", "Sunday")] <- "weekday"
```

### Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
par(mfrow = c(2, 1))
activityByDayType <- aggregate(x = activityCompleted$steps, by = list(activityCompleted$interval, activityCompleted$daytype), FUN = mean, na.rm = TRUE)
colnames(activityByDayType) = c("interval", "daytype", "steps")
plot(x = activityByDayType$interval[activityByDayType$daytype == "weekday"], y = activityByDayType$steps[activityByDayType$daytype == "weekday"], type = "l", xlab = "Interval", ylab = "Avg. Number of Steps", main = "Daily Activity Pattern (weekdays)")
plot(x = activityByDayType$interval[activityByDayType$daytype == "weekend"], y = activityByDayType$steps[activityByDayType$daytype == "weekend"], type = "l", xlab = "Interval", ylab = "Avg. Number of Steps", main = "Daily Activity Pattern (weekends)")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

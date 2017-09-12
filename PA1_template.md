# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
daily_steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
hist(daily_steps)

mean_steps <- mean(daily_steps)
abline(v=mean_steps, col="blue")
text(mean_steps, 18, "mean", col="blue")
text(mean_steps, 16, round(mean_steps, 2), col="blue")

median_steps <- median(daily_steps)
abline(v=median_steps, col="red")
text(median_steps, 24, "median", col="red")
text(median_steps, 22, median_steps, col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is the average daily activity pattern?


```r
# Calculate average steps for a time interval
interval_average <-  with(activity, tapply(steps, interval, mean, na.rm = TRUE))

# Make the time scale decimal
hours = as.integer(names(interval_average)) %/% 100
mins = as.integer(names(interval_average)) %% 100
time_scale = hours + mins / 60

plot(time_scale, interval_average, type="l", xlab="Time", ylab="Average steps")

peak_time <- time_scale[which.max(interval_average)]
abline(v=peak_time, col="red")
text(peak_time, 200, round(max(interval_average),2), pos=2, col="red")
peak_time_text <- paste(round(peak_time), ":", peak_time %% 1 * 60, sep="")
text(peak_time, 0, peak_time_text, pos=4, col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Imputing missing values

```r
missing_values <- sum(is.na(activity$steps))
print(paste("Number of rows where steps = NA is", missing_values))
```

```
## [1] "Number of rows where steps = NA is 2304"
```


```r
# Fill in interval values with their mean value
activity_new <- activity
activity_new$steps[is.na(activity$steps)] = as.vector(interval_average)

daily_steps <- with(activity_new, tapply(steps, date, sum, na.rm = TRUE))
hist(daily_steps)

mean_steps <- mean(daily_steps)
abline(v=mean_steps, col="blue")
text(mean_steps, 18, "mean", col="blue")
text(mean_steps, 16, round(mean_steps, 2), col="blue")

median_steps <- median(daily_steps)
abline(v=median_steps, col="red")
text(median_steps, 24, "median", col="red")
text(median_steps, 22, round(median_steps, 2), col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
The mean and mediian are now closer and there are less entries with a low number of steps.

## Are there differences in activity patterns between weekdays and weekends?


```r
day_types <- c("weekend", "weekday", "weekday", "weekday",
               "weekday", "weekday", "weekend")
activity_new$day_type <- sapply(activity_new$date, 
                                function(x) day_types[as.POSIXlt(x)$wday + 1])

weekday <- subset(activity_new, day_type == "weekday")
weekend <- subset(activity_new, day_type == "weekend")
weekday_average <-  with(weekday, tapply(steps, interval, mean, na.rm = TRUE))
weekend_average <-  with(weekend, tapply(steps, interval, mean, na.rm = TRUE))

par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(time_scale, weekend_average, type="l", xlab="Interval", ylab="Number of steps", main="Weekend")
plot(time_scale, weekday_average, type="l", xlab="Interval", ylab="Number of steps", main="Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

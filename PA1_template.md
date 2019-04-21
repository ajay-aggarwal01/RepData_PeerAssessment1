---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Unzip data to obtain a csv file.


```r
destfile="activity.csv"    

## extract data 

if(!file.exists(destfile)){
  
  URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
  download.file(url = URL, destfile = "repdata_data_activity.zip") 
  unzip("repdata_data_activity.zip")
  
}

library(dplyr)
library(ggplot2)

my_data <- read.csv(destfile)
my_data$date <- as.Date(my_data$date)
str(my_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(my_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
## Q1: Calculate the total number of steps taken per day

```r
library(dplyr)
library(ggplot2)
steps_per_days <- with(my_data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(steps_per_day) <- c( "date" , "total_steps")
steps_per_day$total_steps <- as.numeric(steps_per_day$total_steps)
steps_per_day$total_steps
```
## Q2: histogram of the total number of steps taken each day

```r
hist(steps_per_day$total_steps, col="blue",main = "Total steps taken per day", xlab = "steps counts")
```

## Q3: Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(steps_per_day$total_steps)
median_steps <- median(steps_per_day$total_steps)

mean_steps
median_steps
```



## What is the average daily activity pattern?

### Q4: Make a time series plot type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_pattern <- with(my_data, aggregate(steps, by = list(interval), FUN = mean, na.rm = TRUE))
names(avg_pattern) <- c( "interval" , "avg_steps")
avg_pattern$avg_steps <- as.numeric(avg_pattern$avg_steps)

plot(avg_pattern$interval, avg_pattern$avg_steps, type = "l", col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

### Q5: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
x <- which.max(avg_pattern$avg_steps)
my_interval <- avg_pattern[x,]$interval
max_steps <- max(avg_pattern$avg_steps)

my_interval
```


## Imputing missing values

### Q6: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA)


```r
#missing_steps_counts <- count(filter(my_data, is.na(steps)))
## simpler code
missing_steps_counts <- sum(is.na(my_data$steps))
```



## Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
mean_interval_by_date <- summarise( group_by(my_data, date,interval), mean_steps = mean(steps), na.rm = TRUE)

my_value <- function(date,interval,steps) {
  x <- 'NA'
  if (!is.na(steps))
  {
     x <- c(steps)
  }
  else
  {
    x <- filter(mean_interval_by_date, date==date&interval==interval)
    x <- x$mean_steps

  }
    
  return(x)  
}

my_data$steps <- mapply(my_value, my_data$date, my_data$interval,  my_data$steps)
```

### Q9:Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps 
### taken per day. Do these values differ from the estimates from the first part of the assignment? \


```r
steps_per_days2 <- with(my_data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(steps_per_day) <- c( "date" , "total_steps")
steps_per_day2$total_steps <- as.numeric(steps_per_day$total_steps)

## istogram of the total number of steps taken each day
##barplot(steps_per_day$sum_per_day)
hist(steps_per_day2$total_steps, col="green",main = "Total steps taken per day", xlab = "steps counts")
```

### The mean and median total number of steps  taken per day.


```r
mean_steps <- mean(steps_per_day2$total_steps)
median_steps <- median(steps_per_day2$total_steps)
```


## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
my_data <- read.csv(destfile)

my_data$date <- as.Date(my_data$date)

weekday_check <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

my_data$day <- sapply(my_data$date, FUN=weekday_check)

report_data <- summarise( group_by(my_data, day), mean_steps = mean(steps), na.rm = TRUE)


report_data <- aggregate(steps~interval + day, my_data, mean, na.rm = TRUE)
```

### Make a panel plot containing a time series plot type="l" of the 5-minute interval (x-axis) and 
### the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
ggplot(report_data, aes(x = interval , y = steps, color = day)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~day, ncol = 1, nrow=2)
```

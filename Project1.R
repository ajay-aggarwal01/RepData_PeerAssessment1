## Author: Ajay Aggarwal
## Date: 04/20/2019
##
## The data for this assignment can be downloaded from the course web site:
## Dataset: Activity monitoring data [52K]https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
## The variables included in this dataset are:
## steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
## date: The date on which the measurement was taken in YYYY-MM-DD format
## interval: Identifier for the 5-minute interval in which measurement was taken
## The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
## =========================================================================================================================

setwd("D:/Work/mystuff/Education/DataScience/Reproducible Research")

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



## Q1: Calculate the total number of steps taken per day
steps_per_days <- with(my_data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(steps_per_day) <- c( "date" , "total_steps")
steps_per_day$total_steps <- as.numeric(steps_per_day$total_steps)


## Q2: histogram of the total number of steps taken each day
##barplot(steps_per_day$sum_per_day)
png("plot1.png")
hist(steps_per_day$total_steps, col="blue",main = "Total steps taken per day", xlab = "steps counts")
dev.off()


## Q3: Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day <-  with(steps_per_day, aggregate(total_steps, by = list(date), FUN = mean, na.rm = TRUE))
names(mean_steps_per_day) <- c( "date" , "mean_steps")

mean_steps_per_day <-  with(steps_per_day, aggregate(total_steps, by = list(date), FUN = median, na.rm = TRUE))
names(median_steps_per_day) <- c( "date" , "median_steps")
report_data <- cbind(steps_per_day, mean_steps=mean_steps_per_day$mean_steps, median_steps=median_steps_per_day$median_steps)
names(report_data) <- c( "date" , "total_steps", "mean_steps","median_steps")


##Q4: Make a time series plot type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
## 

avg_pattern <- with(my_data, aggregate(steps, by = list(interval), FUN = mean, na.rm = TRUE))
names(avg_pattern) <- c( "interval" , "avg_steps")
avg_pattern$avg_steps <- as.numeric(avg_pattern$avg_steps)

png("plot2.png")
plot(avg_pattern$interval, avg_pattern$avg_steps, type = "l", col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
dev.off()


## Q5: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
x <- which.max(avg_pattern$avg_steps)
my_interval <- avg_pattern[x,]$interval
my_interval


## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (NA). 
## The presence of missing days may introduce bias into some calculations or summaries of the data.

## Q6: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA
## ======================================================================================================================================

missing_steps_counts <- count(filter(my_data, is.na(steps)))
## simpler code
missing_steps_counts <- sum(is.na(my_data$steps))
missing_steps_counts




## Q7: Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## calculate Mean of interval for a day



## Filter out missing steps
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

## Q8: Create a new dataset that is equal to the original dataset but with the missing data filled in.

my_data$steps <- mapply(my_value, my_data$date, my_data$interval,  my_data$steps)


##
## Q9:Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps 
## taken per day. Do these values differ from the estimates from the first part of the assignment? \
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps_per_days2 <- with(my_data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(steps_per_day) <- c( "date" , "total_steps")
steps_per_day2$total_steps <- as.numeric(steps_per_day$total_steps)

## istogram of the total number of steps taken each day
##barplot(steps_per_day$sum_per_day)
png("plot3.png")
hist(steps_per_day2$total_steps, col="green",main = "Total steps taken per day", xlab = "steps counts")

dev.off()

## The mean and median total number of steps  taken per day.
mean(steps_per_day2$total_steps)
median(steps_per_day2$total_steps)


##Q10 Are there differences in activity patterns between weekdays and weekends?

## Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

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


## Make a panel plot containing a time series plot type="l" of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


report_data <- aggregate(steps~interval + day, my_data, mean, na.rm = TRUE)

png("plot4.png")

plot<- ggplot(report_data, aes(x = interval , y = steps, color = day)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~day, ncol = 1, nrow=2)
print(plot)
dev.off()



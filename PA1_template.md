# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())

```(R)
setwd("C:/coursera/rrassig1")

datafile <- "activity.zip"
if (!file.exists(datafile)){
  fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileurl, destfile = "c:/coursera/rrassig1/activity.zip")
  unzip("c:/coursera/rrassig1/activity.zip", exdir = "C:/coursera/rrassig1/activity")
} else {
  unzip("c:/coursera/rrassig1/activity.zip", exdir = "C:/coursera/rrassig1/activity")
}

activity_data <- read.csv("./activity/activity.csv")

dim(activity_data)
## [1] 17568     3
names(activity_data)
## [1] "steps"    "date"     "interval"
summary(activity_data)
## steps                date          interval     
## Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
## 1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
## Median :  0.00   2012-10-03:  288   Median :1177.5  
## Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
## 3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
## Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
## NA's   :2304     (Other)   :15840  
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

``` (R)
class(activity_data$steps)
## [1] "integer"
class(activity_data$date)
## [1] "factor"
class(activity_data$interval)
## [1] "integer"

activity_data$steps <- as.numeric(activity_data$steps)
activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")
activity_data$interval <- as.factor(activity_data$interval)
str(activity_data)
## 'data.frame':	17568 obs. of  3 variables:
## $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
## $ date    : Date, format: "2012-10-01" "2012-10-01" ...
## $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

``` (R)
totalsteps <- aggregate(steps ~ date, data = activity_data , FUN= sum, na.rm =TRUE )
head(totalsteps)
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```(R)
hist(totalsteps$steps, main = "Histogram of the total number of steps taken each day" , xlab = "Steps per day", col = "yellow" )

barplot(totalsteps$steps, names.arg =totalsteps$date, main = "Histogram of the total number of steps taken each day", xlab = "Date",ylab = "Steps per day", col = "magenta")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```(R)
mean_totalsteps <- mean(totalsteps$steps)
## [1] 10766.19
median_totalsteps <- median(totalsteps$steps)
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```(R)
ave_totalsteps <- aggregate(activity_data$steps, by =list(activity_data$interval), FUN= mean, na.rm =TRUE )

colnames(ave_totalsteps)<-c("interval", "mean")

head(ave_totalsteps)
##   interval      mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396

plot(ave_totalsteps$interval, ave_totalsteps$mean, type = "l",  main = "Average Steps Time Series Plot", xlab = "Interval", ylab = "Total average steps", col = "darkgrey")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```(R)
max_ave_totalsteps <- ave_totalsteps[which.max(ave_totalsteps$mean),]
##     interval     mean
## 104      835 206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` (R)
num_na_data <- sum(is.na(activity_data$steps))
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` (R) 
  a. duplicate the original data
  b. fill the missing values by the mean of the steps column.
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` (R)
## New data frame
fill_activity_data <- activity_data

## filling with the mean the missing values
fill_activity_data$steps[which(is.na(fill_activity_data$steps))] <- mean(fill_activity_data$steps, na.rm = TRUE)

num_na_data2 <- sum(is.na(fill_activity_data$steps))
## [1] 0
## Zero rows with NAs.

str(fill_activity_data)
## 'data.frame':	17568 obs. of  3 variables:
## $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
## $ date    : Date, format: "2012-10-01" "2012-10-01" ...
## $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` (R)
totalsteps_fill <- aggregate(steps ~ date, data = fill_activity_data , FUN= sum)

hist(totalsteps_fill$steps, main = "Histogram of the total number of steps taken each day" , xlab = "Steps per day", col = "yellow" )

meanfill <- mean(totalsteps_fill$steps)
## [1] 10766.19
medianfill <- median(totalsteps_fill$steps)
## [1] 10766.19

 There are no differences between the mean and the median with the imputing missing data.
```

## Are there differences in activity patterns between weekdays and weekends? 

For this part the weekdays() function may be of some help here. Use the dataset  with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` (R)
fill_activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")

## creating  a new data frame by adding a "weekday" column
weekday_activity <- data.frame( date =fill_activity_data$date, weekday=tolower(weekdays(fill_activity_data$date)), steps = fill_activity_data$steps, interval = fill_activity_data$interval)

## indicating whether a given date is a weekday or weekend day by adding a "daytype"column
weekend_activity <- cbind(weekday_activity, daytype = ifelse(test = weekday_activity$weekday=="sábado"| weekday_activity$weekday == "domingo", yes = "weekend", no = "weekday"))

## the new data frame:
head(weekend_activity)
##         date weekday   steps interval daytype
## 1 2012-10-01   lunes 37.3826        0 weekday
## 2 2012-10-01   lunes 37.3826        5 weekday
## 3 2012-10-01   lunes 37.3826       10 weekday
## 4 2012-10-01   lunes 37.3826       15 weekday
## 5 2012-10-01   lunes 37.3826       20 weekday
## 6 2012-10-01   lunes 37.3826       25 weekday
``` 

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` (R)
weekend_activity_mean <- aggregate(weekend_activity$steps, by = list(weekend_activity$daytype, weekend_activity$weekday, weekend_activity$interval), mean) 

colnames(weekend_activity_mean) <- c("daytype", "weekday", "interval", "mean")

head(weekend_activity_mean)

library(lattice)
xyplot(mean~interval| daytype, weekend_activity_mean, type= "l", xlab= "Interval", ylab ="Total Steps", layout=c(1,2))
```

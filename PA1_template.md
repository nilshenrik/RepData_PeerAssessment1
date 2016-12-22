---
title: "Peer Graded Assignment: Course Project 1"
output: html_document
keep_md: true
---


## Loading and preprocessing the data

```r
temp <- tempfile()
download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp, mode="wb")
list.files <- unzip(temp,list=TRUE)

dataset<- list.files$Name[1]
unzip(temp, dataset)
data<-read.csv(dataset)
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```r
library(dplyr)

SumStepsDay<- data %>%
                group_by(date) %>%
                summarise(SumSteps=sum(steps))
```

Make a histogram of the total number of steps taken each day

```r
with(SumStepsDay, hist(SumSteps))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


Calculate and report the mean and median of the total number of steps taken per day

```r
with(SumStepsDay, mean(SumSteps, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
with(SumStepsDay, median(SumSteps, na.rm = TRUE))
```

```
## [1] 10765
```

##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
AverageStepsIntevals<- data %>%
                        group_by(interval) %>%
                        summarise(AveStepInt=mean(steps, na.rm=TRUE))

with(AverageStepsIntevals, plot(interval,AveStepInt, type="l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
AverageStepsIntevals[which(AverageStepsIntevals$AveStepInt==max(AverageStepsIntevals$AveStepInt)),]
```

```
## # A tibble: 1 × 2
##   interval AveStepInt
##      <int>      <dbl>
## 1      835   206.1698
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
        - The imputation method used is MICE.

```r
library("mice")
ini <- mice(data[,-2],m=5,maxit=5,meth='pmm',seed=1) 
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   1   4  steps
##   1   5  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   2   4  steps
##   2   5  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   3   4  steps
##   3   5  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   4   4  steps
##   4   5  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   5   4  steps
##   5   5  steps
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data2<- (complete(ini, action = 1)+complete(ini, action = 2)+complete(ini, action = 3)+complete(ini, action = 4)+complete(ini, action = 5))/5

data2$date<- data$date
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
SumStepsDay<- data2 %>%
        group_by(date) %>%
        summarise(SumSteps=sum(steps))


with(SumStepsDay, hist(SumSteps, breaks= 15))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
with(SumStepsDay, mean(SumSteps, na.rm = TRUE))
```

```
## [1] 10834.23
```

```r
with(SumStepsDay, median(SumSteps, na.rm = TRUE))
```

```
## [1] 11015
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

- Yes, they do differ. Both are higher 

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
data2$date<-as.Date(data2$date,
                   "%Y-%m-%d"
)
Sys.setlocale("LC_TIME",
              "English"
)
```

```
## [1] "English_United States.1252"
```

```r
data3 <- data2 %>% 
        mutate(day= weekdays(date)) %>%
        mutate(weekend=replace(day, day %in% c("Sunday", "Saturday"), "weekend")) %>%
        mutate(weekend=replace(weekend, weekend %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday")) %>%
        group_by(weekend, interval) %>%
        summarise(steps=mean(steps))
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(lattice)

xyplot(steps~interval|factor(weekend),
       data= data3,
       type='l',layout=c(2,1),
       xlab = "Intervals",
       ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data

*I manually downloaded the data from the link below and set the working* 
*directory to the folder containing the file:* *https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip*


```r
#Load the data 
activity <- read.csv("activity.csv")

#Process/transform the data into a format suitable for your analysis
activity$date <- as.Date(activity$date,"%Y-%m-%d")
numberObs <- length(activity$steps) 
```

## What is mean total number of steps taken per day?


```r
#Calculate the total number of steps taken per day
totalSteps <- tapply(activity$steps,activity$date,sum)
totalSteps <- totalSteps[!is.na(totalSteps)]

#Make a histogram of the total number of steps taken each day
hist(totalSteps, main = "Total Steps per Day",xlab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Calculate and report the mean and median of the total number of steps taken per 
#day
paste("The mean steps per day is",format(mean(totalSteps),nsmall=2))
```

```
## [1] "The mean steps per day is 10766.19"
```

```r
paste("The median steps per day is",median(totalSteps))
```

```
## [1] "The median steps per day is 10765"
```

## What is the average daily activity pattern?


```r
#Make a time series plot of the 5-minute interval (x-axis) and the average 
#number of steps taken, averaged across all days (y-axis)
meanSteps <- tapply(activity$steps,activity$interval,mean,na.rm = TRUE)
plot(names(meanSteps),meanSteps,type="l",ylab = "Average Steps",xlab = "Time of Day")
title(main = "Average Steps by Time of Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Which 5-minute interval contains the maximum number of steps?
maxTime <- names(meanSteps[meanSteps==max(meanSteps)])
paste("The maximum average number of steps occurs at",maxTime)
```

```
## [1] "The maximum average number of steps occurs at 835"
```

## Imputing missing values


```r
#Calculate and report the total number of missing values in the dataset
totalNA <- sum(is.na(activity$steps))
paste("The total number of missing values is",totalNA)
```

```
## [1] "The total number of missing values is 2304"
```

```r
#Create a new dataset with the missing data filled in. 
#I imputed the mean number of steps for each interval.
imputedActivity <- activity
for(i in 1:numberObs){
        if (is.na(imputedActivity$steps[i])){
                 imputedActivity$steps[i]<-meanSteps[activity$interval[i]==names(meanSteps)]
        }
}
totalImputedSteps <- tapply(imputedActivity$steps,imputedActivity$date,sum)
hist(totalImputedSteps, main = "Total Steps per Day (Imputing Missing Values)",
     xlab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
paste("The mean steps per day is",format(mean(totalImputedSteps),nsmall=2))
```

```
## [1] "The mean steps per day is 10766.19"
```

```r
paste("The median steps per day is",format(median(totalImputedSteps),nsmall=2))
```

```
## [1] "The median steps per day is 10766.19"
```

After imputing the missing values, the mean number of steps per day is the same 
as before. However, the median is now the same as the mean.

## Are there differences in activity patterns between weekdays and weekends?


```r
#Create a new factor variable in the dataset with two levels - "weekday" and 
#"weekend" 
weekActivity <- imputedActivity
for (i in 1:numberObs){
        if(weekdays(weekActivity$date[i])=="Saturday"){
                weekActivity$weekday[i] <- "weekend"  
        }
        else if(weekdays(weekActivity$date[i])=="Sunday"){
                weekActivity$weekday[i] <- "weekend"  
        } 
        else{
                weekActivity$weekday[i] <- "weekday"
        }
}

#Make a panel plot containing a time series plot averaged across all weekday days 
#or weekend days

weekdayActivity <- weekActivity[weekActivity$weekday == "weekday",]
weekendActivity <- weekActivity[weekActivity$weekday == "weekend",]

meanWeekdaySteps <- tapply(weekdayActivity$steps,weekdayActivity$interval,mean)
plot(names(meanWeekdaySteps),meanWeekdaySteps,type="l",ylab = "Average Steps",xlab = "Time of Day")
title(main = "Average Steps on Weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
meanWeekendSteps <- tapply(weekendActivity$steps,weekendActivity$interval,mean)
plot(names(meanWeekendSteps),meanWeekendSteps,type="l",ylab = "Average Steps",xlab = "Time of Day")
title(main = "Average Steps on Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

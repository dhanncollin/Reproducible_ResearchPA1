---
title: "Reproducible Research Project 1"
author: "Dhann Collin Davies Vergara"
date: "December 29, 2018"
output: html_document
---
## Introduction

It is now possible to collect a large amount of data abput personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
  
  * Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:
  
  * steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
  * date: The data on which the measurements was taken in YYYY-MM-DD format
  * interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

1. Load the needed packages
```{r}
library(data.table)
library(ggplot2)
```

2. Load the activity.csv data 
```{r}
activitydata <- data.table::fread(input = "activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```{r}
steps <- activitydata[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
```

2. Make a histogram of the total number of steps taken each day. 

```{r}
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "pink", binwidth = 1000) +
  labs(title = "Total Steps per Day", x = "Steps", y = "Frequency")
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
steps[, .(Mean = mean(steps, na.rm = TRUE), Median = median(steps, na.rm = TRUE))]
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type ="1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
intervaldata <- activitydata[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(intervaldata, aes(x = interval , y = steps)) + geom_line(color="yellowgreen", size=1) + labs(title = "Average Daily Steps per Interval", x = "Interval", y = "Average Daily Steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  
```{r}
intervaldata[steps == max(steps), .(maxinterval = interval)]
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
nrow(activitydata[is.na(steps),])
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
# Filling the missing data using the median datasets
activitydata[is.na(steps), "steps"] <- activitydata[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
data.table::fwrite(x = activitydata, file = "newdata.csv", quote = FALSE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
```{r}
# Steps taken each day 
steps <- activitydata[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)]

# The mean and median of steps taken each day
steps[, .(Mean = mean(steps), Median = median(steps))]
ggplot(steps, aes(x = steps)) + geom_histogram(fill = "lightblue", binwidth = 1000) + labs(title = "Total Steps per Day", x = "Steps", y = "Frequency")
```

There is a difference on the values of the total daily steps before and after imputing the missing data using the median dataset

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
activitydata <- data.table::fread(input = "activity.csv")
activitydata[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activitydata[, `Day of Week`:= weekdays(x = date)]
activitydata[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "Weekday or Weekend"] <- "Weekday"
activitydata[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "Weekday or Weekend"] <- "Weekend"
activitydata[, `Weekday or Weekend` := as.factor(`Weekday or Weekend`)]
```

2. Make a panel plot containing a time series plot (i.e.type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
activitydata[is.na(steps), "steps"] <- activitydata[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
intervaldata <- activitydata[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `Weekday or Weekend`)] 
ggplot(intervaldata, aes(interval, steps)) + geom_line(color="blueviolet", size=1) + facet_grid(~`Weekday or Weekend`) + facet_wrap(~`Weekday or Weekend` , ncol = 1, nrow=2) + labs(title = "Average Daily Steps Based on Type of Day", x = "Interval", y = "No. of Steps") 
```

---
title: "PA1_template"
author: "Jean Simpson"
date: "April 22, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Loading and preprocessing the data

1. Load the data set into R

```{r}
activity <- read.csv("activity.csv")
```

2. Convert the date field

```{r}
activity$date <- as.Date(activity$date)
```



What is the mean total number of step taken per day?

1.Calculate the total number of steps taken per day

```{r}
steps_per_day <- aggregate(steps ~ date, activity, sum)
```

2. Make a histogram of the total number of steps taken each day.

```{r}
hist(steps_per_day$steps, main = paste("Total Number of Steps Per Day"), col="green", xlab="Number of Steps")
```

3. Calculate and report the mean and median of the total number of steps taken per day.

```{r}
rmean <- mean(steps_per_day$steps)
rmedian <- median(steps_per_day$steps)
```

The mean steps taken per day were 10,766. The median steps taken per day were 10,765.




What is the average daily activity pattern?

1. Make a plot of the 5-minute interval and the average number of steps taken, averaged across all the days.

```{r}
steps_by_interval <- aggregate(steps ~ interval, activity, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="5-minute Interval", ylab="Number of Steps", main="Average Number of Steps per Day by Interval")
```

2. Which 5-minute interval contains the maximum number of steps?

```{r}
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

The 835th 5-minute interval contains the maximum number of steps at 206.




Imputing missing values

1. Calculate and report the total number of missing values.

```{r}
missing_values <- sum(is.na(activity$steps))
```

There are 2304 missing values in the data set.

2. Devise a strategy for filling in the missing values.
  
The number of missing values in this dataset is fairly large, so we cannot determine if there is no bias. I will imput missing values based on the mean number of steps for a particular 5-minutes interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activity2 <- transform(activity, steps = ifelse(is.na(activity$steps), steps_by_interval$steps[match(activity$interval, steps_by_interval$interval)], activity$steps))

activity2[as.character(activity2$date) == "2012-10-01", 1] <- 0
```

4. Make a histogram of the new dataset and calculate the mean and median. Do these values differ from the estimates from the fisrt part of the assigment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
steps_per_day2 <- aggregate(steps ~ date, activity2, sum)
hist(steps_per_day2$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

hist(steps_per_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=5)
```

```{r}
rmean2 <- mean(steps_per_day2$steps)
rmedian2 <- median(steps_per_day2$steps)
```

It does not seem that the values differ much from the original data set. Therefore it seems the impact of imputing missing data on the estimates is very little.



Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend".

```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
activity2$dow = as.factor(ifelse(is.element(weekdays(as.Date(activity2$date)),weekdays), "Weekday", "Weekend"))
```

2. Make a panel plot containing a time series of the 5-minute interval and the avg number of steps taken, averages across weekend or weekdays.

```{r}
steps_by_interval2 <- aggregate(steps ~ interval + dow, activity2, mean)
library(lattice)
xyplot(steps_by_interval2$steps ~ steps_by_interval2$interval|steps_by_interval2$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Number of Steps",layout=c(1,2), type="l")
```
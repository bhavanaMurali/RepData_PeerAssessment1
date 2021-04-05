PA1\_template.Rmd
================
Bhavana
4/4/2021

## Loading and preprocessing the data

``` r
activity <- read.csv("activity.csv")
class(activity)
```

    ## [1] "data.frame"

``` r
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
activity_data <- na.omit(activity)
```

## What is mean total number of steps taken per day?

``` r
library(ggplot2)
steps_per_day <- aggregate(activity_data$steps, by = list(Steps.Date = activity_data$date), FUN = "sum")
qplot(x,binwidth=1000,data=steps_per_day,xlab="Date",ylab="Steps",main="Total Steps taken each day")
```

![](figure/directoryunnamed-chunk-2-1.png)<!-- --> Mean and Median are,

``` r
mean(steps_per_day$x)
```

    ## [1] 10766.19

``` r
median(steps_per_day$x)
```

    ## [1] 10765

## What is the average daily activity pattern?

``` r
average <- aggregate(x=list(steps=activity_data$steps),by = list(interval=activity_data$interval),FUN="mean")
plot(average$interval,average$steps,type="l",main="Time series plot",ylab="Average number of steps",xlab="5 min Intervals")
```

![](figure/directoryunnamed-chunk-4-1.png)<!-- -->

The interval with the maximum number of steps is,

``` r
interval_row <- which.max(average$steps)
average[interval_row,1]
```

    ## [1] 835

## Imputing missing values

There are many days/intervals where there are missing values (coded as
`NA`). The presence of missing days may introduce bias into some
calculations or summaries of the data.

Finiding the total number of missing values from the data,

``` r
missing <- is.na(activity$steps)
sum(missing)
```

    ## [1] 2304

Filling all the missing values with the mean of the 5- minute interval,

``` r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (average[average$interval==interval, "steps"])
    return(filled)
}

filled.data <- activity
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval) 
```

Now, using the filled data set, let’s make a histogram of the total
number of steps taken each day and calculate the mean and median total
number of steps.

``` r
per_day <- aggregate(x=list(steps = filled.data$steps), by = list(date = filled.data$date), FUN = "sum")
qplot(steps,binwidth=1000,data=per_day,xlab="Date",ylab="Steps",main="Total Steps taken each day")
```

![](figure/directoryunnamed-chunk-8-1.png)<!-- --> The new mean and
median are,

``` r
mean(per_day$steps)
```

    ## [1] 10766.19

``` r
median(per_day$steps)
```

    ## [1] 10766.19

Mean and median values are similar to the ones before filling the NA
values. The median here is equal to that of the mean.

## Are there differences in activity patterns between weekdays and weekends?

``` r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Plotting the data across weekends and weekdays separately,

``` r
average.data <- aggregate(x=list(steps=filled.data$steps), by=list(interval=filled.data$interval,day=filled.data$day), mean)
ggplot(average.data, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")
```

![](figure/directoryunnamed-chunk-11-1.png)<!-- -->

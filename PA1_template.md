---
title: "Reproducible Research week 2 assignment"
author: "Karsten Petersen"
date: "19 dec 2018"
output: 
  html_document: 
    keep_md: yes
---



## Setup, read, convert data

**load required libraries**

```r
library(dplyr)
library (lattice)
```

**Read data from zipfile on www:**

```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
```

**Explore and convert data**

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

**1. Number of steps:**

```r
steps_day <- data %>%
  filter (!is.na(steps)) %>%
  group_by(date) %>%
  summarise(steps_n=n(),
            steps_sum=sum(steps),
            steps_mean=mean(steps),
            steps_median=median(steps))

steps_day[,c(1,3)] 
```

```
## # A tibble: 53 x 2
##    date       steps_sum
##    <date>         <int>
##  1 2012-10-02       126
##  2 2012-10-03     11352
##  3 2012-10-04     12116
##  4 2012-10-05     13294
##  5 2012-10-06     15420
##  6 2012-10-07     11015
##  7 2012-10-09     12811
##  8 2012-10-10      9900
##  9 2012-10-11     10304
## 10 2012-10-12     17382
## # ... with 43 more rows
```

**2. Histogram for number of steps taken each day**


```r
hist(steps_day$steps_sum, main="2. Histogram for number of steps taken each day",xlab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

**3. Calculate mean and median:**  
Mean and median is already calculated in '1. Number of steps'...

```r
steps_day[,c(1,4,5)]
```

```
## # A tibble: 53 x 3
##    date       steps_mean steps_median
##    <date>          <dbl>        <dbl>
##  1 2012-10-02      0.438            0
##  2 2012-10-03     39.4              0
##  3 2012-10-04     42.1              0
##  4 2012-10-05     46.2              0
##  5 2012-10-06     53.5              0
##  6 2012-10-07     38.2              0
##  7 2012-10-09     44.5              0
##  8 2012-10-10     34.4              0
##  9 2012-10-11     35.8              0
## 10 2012-10-12     60.4              0
## # ... with 43 more rows
```

##What is the average daily activity pattern?
**1. Time Series plot**

```r
steps_average_interval <- data %>%
  filter (!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(steps_n=n(),
            steps_sum=sum(steps),
            steps_mean=mean(steps),
            steps_median=median(steps))

plot (steps_average_interval$interval,steps_average_interval$steps_mean, type="l",main="1. Time Series plot",xlab="interval",ylab="average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

**2. Which interval contains maximum number of steps**

```r
steps_average_interval[steps_average_interval$steps_mean==max(steps_average_interval$steps_mean),] 
```

```
## # A tibble: 1 x 5
##   interval steps_n steps_sum steps_mean steps_median
##      <int>   <int>     <int>      <dbl>        <int>
## 1      835      53     10927       206.           19
```

##Imputing missing values##
**1. Missing values in dataset**  
Only 'steps' variable contains missing values.  

```r
NAcolumns <- function(x) any(is.na(x))
apply (data,2,NAcolumns)
```

```
##    steps     date interval 
##     TRUE    FALSE    FALSE
```

Number of NA observations in steps variable:2304   
Missing values in 'steps' are replaced by the median for that interval across all dates

```r
# add average data for each interval by merging
data_impute_NA <- merge(data,steps_average_interval)

# substitute NA with median value for the step in new variable 'steps_no_NA' 
data_impute_NA <- mutate(data_impute_NA,steps=ifelse(is.na(steps),steps_median,steps))

# keep only columns that appears in original file 'activity.csv'
data_impute_NA <- data_impute_NA[,c(2,3,1)] 

steps_day_impute_NA <- data_impute_NA %>%
  group_by(date) %>%
  summarise(steps_n=n(),
            steps_sum=sum(steps),
            steps_mean=mean(steps),
            steps_median=median(steps))
```

**2a. Mean and median**

```r
steps_day_impute_NA[,c(1,4,5)]
```

```
## # A tibble: 61 x 3
##    date       steps_mean steps_median
##    <date>          <dbl>        <dbl>
##  1 2012-10-01      3.96             0
##  2 2012-10-02      0.438            0
##  3 2012-10-03     39.4              0
##  4 2012-10-04     42.1              0
##  5 2012-10-05     46.2              0
##  6 2012-10-06     53.5              0
##  7 2012-10-07     38.2              0
##  8 2012-10-08      3.96             0
##  9 2012-10-09     44.5              0
## 10 2012-10-10     34.4              0
## # ... with 51 more rows
```

**2b. Histogram for number of steps taken each day NA's imputed**  
The imputation of NA's increases the frequency of observations with 0-5000 step  

```r
hist(steps_day_impute_NA$steps_sum, main="2. Histogram for number of steps taken each day (NA's imputed)",xlab="Number of steps ",cex.main=0.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

##Are there differences in activity patterns between weekdays?  
There is less activity at the start of the day, but later in the day there is more activity in the weekends!

```r
# identify day of the week
data_impute_NA <- mutate(data_impute_NA, weekday= weekdays(date,abbreviate=F)) 

# identify weekend or weekday
data_impute_NA <- mutate(data_impute_NA,weekend=ifelse(weekday %in% c("lørdag","søndag"),"weekend","weekday"))

# calculate averages pr interval 
steps_average_interval_impNA <- data_impute_NA %>%
  group_by(interval, weekend) %>%
  summarise(steps_mean=mean(steps))
 
#convert to factor
steps_average_interval_impNA$weekend <- as.factor(steps_average_interval_impNA$weekend)

# lattice plot  
with(steps_average_interval_impNA, xyplot(steps_mean ~ interval   | weekend,
  xlab="Interval",
  ylab="Number of steps",
  type="l",
  layout=c(1,2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


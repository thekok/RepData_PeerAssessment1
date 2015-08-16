
#Reproducible Research Peer Assessment 1

##Loading and preprocessing the data
  

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.1
```

```r
# url to download from
fileurl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# create temp folder
td = "D:\\Temp\\Coursera\\Module5"

# placeholder file
tf = tempfile(tmpdir=td, fileext=".zip")

# download to placeholder file
download.file(fileurl, tf)

# get the name of the first file in the zip archive
fname = unzip(tf, list=TRUE)$Name[1]

# unzip the file to the temp folder
unzip(tf, files=fname, exdir=td, overwrite=TRUE)

# fpath is the full path to the file
fpath = file.path(td, fname)

# load the data to data frame
df <- read.csv(fpath, as.is=TRUE)
```

##What is mean total number of steps taken per day?


```r
# generate df2 with complete cases only
df2 <- na.omit(df)

# aggregate steps as per date to get total number of steps in a day
table_date_steps <- aggregate(steps ~ date, df2, sum)

# create histogram of total number of steps in a day
hist(table_date_steps$steps, col=1, main="Histogram - total number of steps per day", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

##mean - total number of steps per day


```r
# mean total number of steps per day
mean(table_date_steps$steps)
```

```
## [1] 10766.19
```

##median - total number of steps per day


```r
# median total number of steps per day
median(table_date_steps$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?


```r
# aggregate by steps steps
table_interval_steps <- aggregate(steps ~ interval, df2, mean)

# generate the line plot of the 5-minute interval (x-axis) and the average number of 
# steps taken, averaged across all days (y-axis)
plot(table_interval_steps$interval, table_interval_steps$steps, type='l', col=1, 
     main="Avg num of steps averaged over all days", xlab="Interval", 
     ylab="Avg num of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

##5-minute interval, that contains the maximum number of steps


```r
# find row id of maximum average number of steps in an interval
max_ave_steps_row_id <- which.max(table_interval_steps$steps)

# get the interval with maximum average number of steps in an interval
table_interval_steps [max_ave_steps_row_id, ]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values


```r
# get rows with NA's
df_NA <- df[!complete.cases(df),]

# number of rows
nrow(df_NA)
```

```
## [1] 2304
```

##Imputed missing values - Histogram



```r
# perform the imputation
for (i in 1:nrow(df)){
  if (is.na(df$steps[i])){
    interval_val <- df$interval[i]
    row_id <- which(table_interval_steps$interval == interval_val)
    steps_val <- table_interval_steps$steps[row_id]
    df$steps[i] <- steps_val
  }
}

# aggregate steps as per date to get total number of steps in a day
table_date_steps_imputed <- aggregate(steps ~ date, df, sum)

# create histogram of total number of steps in a day
hist(table_date_steps_imputed$steps, col=1, main="(Imputed) Histogram of total number of steps per day", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

##mean - Imputing missing values - total number of steps per day


```r
# mean total number of steps per day
mean(table_date_steps_imputed$steps)
```

```
## [1] 10766.19
```

##median - Imputing missing values - total number of steps per day


```r
# median total number of steps per day
median(table_date_steps_imputed$steps)
```

```
## [1] 10766.19
```

###After data imputation, means same, median different.

##Are there differences in activity patterns between weekdays and weekends?


```r
# convert date from string to Date class
df$date <- as.Date(df$date, "%Y-%m-%d")

# add a new column indicating day of the week 
df$day <- weekdays(df$date)

# add a new column called day type and initialize to weekday
df$day_type <- c("weekday")

# If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(df)){
  if (df$day[i] == "Saturday" || df$day[i] == "Sunday"){
    df$day_type[i] <- "weekend"
  }
}

# convert day_time from character to factor
df$day_type <- as.factor(df$day_type)

# aggregate steps as interval to get average number of steps in an interval across all days
table_interval_steps_imputed <- aggregate(steps ~ interval+day_type, df, mean)

# make the panel plot for weekdays and weekends

qplot(interval, steps, data=table_interval_steps_imputed, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day_type, ncol=1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


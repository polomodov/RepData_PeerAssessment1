# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
dataDirectory <- "data"
zipFile <- paste(".", ".", "activity.zip", sep = "/")
unzippedFile <- paste(".", dataDirectory, "activity.csv", sep = "/")
# create directory for data
if (!file.exists(dataDirectory)) {
    dir.create(dataDirectory)
}
# check existing unzipped file
if (file.exists(zipFile) & !file.exists(unzippedFile)) {
    unzip(zipfile = zipFile, exdir = dataDirectory)
}
data <- read.csv(unzippedFile)

head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```



## What is mean total number of steps taken per day?
### Aggregate by date and create histogram

```r
dataAggregatedByDate <- aggregate(steps ~ date, FUN = sum, data = data)
hist(dataAggregatedByDate$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### Calculate Mean

```r
mean(dataAggregatedByDate$steps, na.rm = TRUE)
```

```
## [1] 10766
```

### Calculate Median

```r
median(dataAggregatedByDate$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
### Aggregate data by interval and create time plot

```r
dataAggregatedByInterval <- aggregate(steps ~ interval, FUN = mean, data = data)
plot(dataAggregatedByInterval$interval, dataAggregatedByInterval$steps, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

### Find interval with maximum number of steps

```r
dataAggregatedByIntervalSorted <- dataAggregatedByInterval[order(dataAggregatedByInterval$steps, 
    decreasing = T), ]
dataAggregatedByIntervalSorted[1, "interval"]
```

```
## [1] 835
```


## Imputing missing values
### Calculate total number of rows with NAs

```r
naData <- data[is.na(data[, 1]), ]
nrow(naData)
```

```
## [1] 2304
```


### Filling data strategy
- Fill NA value with mean steps value for that day
- If it's still empty when use average value for that interval

```r
dataFilling = data
dataMeanByDate <- aggregate(steps ~ date, FUN = mean, data = data)
for (row in 1:length(dataFilling$interval)) {
    if (is.na(dataFilling[row, 1])) {
        dataFilling[row, 1] = dataMeanByDate[row, 2]
    }
    if (is.na(dataFilling[row, 1])) {
        dataFilling[row, 1] = dataAggregatedByInterval[row, 2]
    }
}
```

### Make histrogram for restored data
Histogram shape is the same

```r
dataFillingAggregatedByDate <- aggregate(steps ~ date, FUN = sum, data = dataFilling)
hist(dataFillingAggregatedByDate$steps)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

### Calculate mean
Mean is very close to original

```r
mean(dataFillingAggregatedByDate$steps, na.rm = TRUE)
```

```
## [1] 10802
```

### Calculate median
Median is very close to original

```r
median(dataFillingAggregatedByDate$steps, na.rm = TRUE)
```

```
## [1] 10890
```



## Are there differences in activity patterns between weekdays and weekends?
- Create factor variable and separate data to 2 categories
- Make plots for these categories
- Significant difference exist in number of steps by periods:
On weekends people wake up later but more active during day especially after morning peak

```r
# use lubridate library to get day number
library(lubridate)
dataWithWeekday <- data
daysType <- wday(as.Date(dataWithWeekday$date))
wd <- daysType == "1" | daysType == "7"
daysType[wd] <- "Weekend"
daysType[!wd] <- "Weekday"
daysType <- as.factor(daysType)
dataWithWeekday <- cbind(dataWithWeekday, daysType)
dataAggregatedByIntervalWithWeekday <- aggregate(steps ~ interval + daysType, 
    FUN = mean, data = dataWithWeekday)

library("lattice")
xyplot(steps ~ interval | daysType, data = dataAggregatedByIntervalWithWeekday, 
    type = "l", layout = c(1, 2))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


### create html

```r
knit2html()
```

```
## Error: object 'input2' not found
```


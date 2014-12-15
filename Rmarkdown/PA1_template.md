---
title: "Peer Assesment1"

output: html_document
---

This document is to analyze the data collected from a device that collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


So first of all we read the data into csv file

```r
data <- read.csv("activity.csv")
```

The data is converted into a dataframe and missing values removed

```r
data_frame <- as.data.frame(data)
# remove missing values
clean_data <- na.omit(data_frame)
# Compute the total number of steps taken each day
agg <- aggregate(steps ~ date, clean_data, sum)
```
A histogram displays the frequency of the Total number of steps/day

```r
hist(agg[, 2] , right=FALSE, main = "Total no. of steps taken/day", xlab = "total no. of steps/day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculating mean & median of steps per day


```r
Mean <- mean(agg$steps)
Mean
```

```
## [1] 10766.19
```

```r
Median <- median(agg$steps)
Median
```

```
## [1] 10765
```


Finding the average daily activity pattern

```r
splitdf2 <- split(data_frame[, 1], data_frame$date)
df3 <- as.data.frame(splitdf2)
index <- 1:288
x <- 2:289
z <- NULL
for(i in seq(along=x)) {
        if(x[i] < 290) {
                z <- c(z, mean(na.omit(as.numeric(df3[x[i]-1, ])))) 
        } else {
                z  
        }
}

table <- cbind(index,z)
```
Illustraing using a x-y plot


```r
plot(table, type="l", xlab= "5-min interval", ylab="no. of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The 5-minute interval that contains the maximum number of steps is

```r
table_DF <- as.data.frame(table)
max_steps <- max(table_DF$z)
which(table_DF$z == max_steps)
```

```
## [1] 104
```

```r
#206 at 104 interval at 8:40 a.m
```

Dealing with missing values

The total number of missing values in the dataset

```r
Total <- nrow(data_frame)
complete_sum <- sum(complete.cases(data_frame))
no_missing_values<- Total  - complete_sum 
no_missing_values
```

```
## [1] 2304
```
Filling in missing values with 5-min interval mean

```r
df4 <- df3

ind <- which(is.na(df4), arr.ind=TRUE)
df4[ind] <- rowMeans(df4,  na.rm = TRUE)[ind[,1]]
```
mean & median after replacing missing values

```r
col_list <- colSums(df4)
New_mean <- mean(col_list)   
New_median <- median(col_list)
```
Make a histogram of the total number of steps taken each day 

```r
hist(colSums(df4) , right=FALSE, main = "Adjusted histogram after replacing missing values", xlab = "total no. of steps/day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
Previosuly mean was higher than the median, but after replacing outliers with means of 5-min intervals, the mean and the median are now equal



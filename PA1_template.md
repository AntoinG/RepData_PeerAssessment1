---
title: "PA1_template.md"
author: "Antoin van Gemert"
date: "6 January 2018"
output: html_document
---
#1. Loading and preprocessing the data
## 1.1 the data is loaded into variable 'act', and checks have been performed using the functions 'summary' and 'head'
## 1.2 in this stage no further processing of the data is necessary
```{r}
act <- read.csv("./data/activity.csv")
```
```{r}
summary(act)
```
```{r}
head(act)
```
#2. What is the mean total number of steps taken per day?
### For this part of the assignment, you can ignore the missing values in the dataset.
## 2.1 Calculate the total number of steps taken per day. The result is sored in variable 'spd'.
```{r}
spd <- aggregate(steps ~ date, act, sum, na.rm = TRUE)
```
```{r}
head(spd)
```
## 2.2 Make a histogram of the total number of steps taken each day.
```{r}
hist(spd$steps, col = "red", xlab = "Steps", main = "Steps per Day")
```
## 2.3 Calculate and report the mean and median of the total number of steps taken per day.
### The mean total number of steps is stored in variable 'meanspd'
```{r}
meanspd <- mean(spd$steps)
```
```{r}
meanspd
```
### The median total number of steps is stored in variable 'medspd'
```{r}
medspd <- median(spd$steps)
```
```{r}
medspd
```
#3. What is the average daily activity pattern?
## 3.1 Make a time series plot (i.e) of the 5-minute interval (x - axis) and the average number of steps taken, averaged across all days (y - axis).
```{r}
spi <- aggregate(steps ~ interval, data = act, mean, na.rm = TRUE)
```
```{r}
plot(steps ~ interval, data = spi, type = "l", xlab = "5-minute interval", ylab = "Average number of steps", main = "Average Daily Activity Pattern")
```
## 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
### The 5-minute interval with the max number of steps is sored in variable 'maxsteps". 
```{r}
maxsteps <- spi[which.max(spi$steps), ]$interval
```
```{r}
maxsteps
```
#4. Imputing missing values
## 4.1 Calculate and report the total number of missing values in the dataset(i.e. the total number of rows with s)
### The variable 'misval'contains the number of missing values.
```{r}
misval <- sum(is.na(act$steps))
```
```{r}
misval
```
## 4.2 Devise a strategy for filling in all the missing values in the dataset. The strategy does not need to be sophistacated.
```{r}
mspi <- function(interval){
  spi[spi$interval == interval, ]$steps
}
```
## 4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
### actnona contains the new data set with the missing data filled in
```{r}
actnona <- act
```
```{r}
for (i in 1:nrow(actnona)) {
  if(is.na(actnona[i,]$steps)){
    actnona[i,]$steps <- mspi(actnona[i,]$interval)
  }
}
```
### Checking if all NA's are filled
```{r}
misval <- sum(is.na(actnona$steps))
```
```{r}
misval
```
## 4.4 Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.
### histogram
```{r}
spdnona <- aggregate(steps ~ date, data = actnona, sum) 
```
```{r}
hist(spdnona$steps, col = "red", xlab = "Steps", main = "Steps per Day Complete")
```
### new mean
```{r}
meanspdnona <- mean(spdnona$steps)
```
```{r}
meanspdnona
```
### new median
```{r}
medspdnona <- median(spdnona$steps)
```
```{r}
medspdnona
```
### now the new mean equals the new median 
```{r}
meanspdnona == medspdnona
```     
# 5. Are there differences in activity patterns weekdays and weekends?
## 5.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
###(My computer uses local settings. To create the variable 'weekend' local names of weekdays are used, "zaterdag" means "Saturday", "zondag" means "Sunday").
```{r}
weekend <- weekdays(as.Date(actnona$date)) %in% c("zaterdag", "zondag")
```
```{r}
actnona$daytype <- "weekday"
```
```{r}
actnona$daytype[weekend == TRUE] <- "weekend"
```
```{r}
actnona$daytype <- as.factor(actnona$daytype)
```
### factor variable with two levels - "weekday" and "weekend".
```{r}
str(actnona)
```
```{r}
head(actnona)
```
```{r}
nwi <- aggregate(steps ~ interval + daytype, actnona, mean)
```
```{r}
names(nwi)[3] <- "mean_steps"
```
```{r}
head(nwi, 5)
```
## 5.2 Make a panel plot containing a time series plot (i.e.) of the 5-minutes interval (x-axis) and the average nimber of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
library(lattice)
```
```{r}
xyplot(mean_steps ~ interval | daytype, nwi, type = "l", layout = c(1,2), main = "Time Series Plot", xlab = "Interval", ylab = "Average Steps Taken")
```

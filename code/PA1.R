##
## Loading and preprocessing the data
##

## 1. Load the data
aData <- read.csv(file=".\\data\\activity.csv",header=TRUE, stringsAsFactors =FALSE)
head(aData)

## 2. Preprocess the data 
## a. Create a factor variable which has weekend or weekday value
library(timeDate)

aData$daytype <- ifelse (isWeekend(strptime(aData$date, "%m/%d/%Y" )), "weekend", "weekday")
aData$daytype <- as.factor(aData$daytype)

## b. Create a dataset with the NA removed
aData.rmNA <- aData[which(!is.na(aData$steps)), ]

##
## Mean total number of steps taken per day
##

## 1. Calculate total number of steps taken per day
totStepsDaily <- tapply(aData.rmNA$steps, as.factor(aData.rmNA$date), sum)

## 2. Create a histogram of the total number of steps taken daily
hist(totStepsDaily, breaks=60)

## 3. Calculate and report the mean and median of the total number of steps taken per day
summary(totStepsDaily)

##
## Average Daily Activity Pattern
##

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all days (y-axis)
y <- tapply(aData.rmNA$steps, as.factor(aData.rmNA$interval), mean)
interval_time <- strptime(sprintf("%04d", as.numeric(names(y))), format="%H%M")
plot(interval_time, y, type="l", xlab = "", ylab = "Mean steps for each 5 minute interval")

## 2.Which 5-minute interval, on average across all the days in the dataset, contains the 
## maximum number of steps?
interval_time[order(y, decreasing=TRUE)[1]]

## 
## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). 
##The presence of missing days may introduce bias into some calculations or summaries of the data.
##

## 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
length(which(is.na(aData$steps)))

## 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## 

## Calculate the mean for each Interval based on non-missing data
meanStepsInterval <- tapply(aData.rmNA$steps, as.factor(aData.rmNA$interval), mean)


## 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
aData.noNA <- aData

## Replace the missing step values with the mean for that interval
for (i in 1:dim(aData.noNA)[1]){
  if (is.na(aData.noNA$steps[i])) {
    aData.noNA$steps[i] <- meanStepsInterval[as.character(aData.noNA$interval[i])]
  }
}


## 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
## total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates
totStepsDaily.noNA <- tapply(aData.noNA$steps, as.factor(aData.noNA$date), sum)
hist(totStepsDaily.noNA, breaks=60)


## Plot the weekday and weekend data
## Summarize the data into intervals and create a panel plot

## Note the intervals vecome the rownames of the vector returned by tapply
pData <- as.data.frame(tapply(aData.noNA$steps, list(as.factor(aData.noNA$interval),aData.noNA$daytype) , mean))
pData$interval_time <- strptime(sprintf("%04d", as.numeric(rownames(pData))), format="%H%M")

## Plot using base plot
par(mfrow=c(2,1))
plot(pData$interval_time, pData$weekday, type="l", xlab = "", ylab = "Mean steps for each interval", main = "Weekday")
plot(pData$interval_time, pData$weekend, type="l", xlab = "", ylab = "Mean steps for each interval", main = "Weekend")

## Reshape the data
l <- reshape(pData, varying = c("weekday", "weekend"), v.names = "steps", timevar = "daytype", times = c("Weekday", "Weekend"),direction = "long")
## l$daytype <- as.factor(l$daytype)
l <- transform(l, daytype = factor(daytype))

## Plot using ggplot
library(ggplot2)
qplot(interval_time, steps, data = l, facets = daytype ~ ., geom=c("line"))




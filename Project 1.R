library(DBI)
library(ggplot2)
library(lubridate)
library(dplyr)
rm(list=ls())
# find the data 
setwd("E:/Data Science/Project 1 reproduciable research")
unzip("./data.zip")
activityData <- read.csv(file="./activity.csv")
summary(activityData)
head(activityData)
#find total number of steps per day 
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
#draw histogram
hist(stepsPerDay$steps,col="red")
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
#find the median steps per day
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
#find the pattern of day
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l",col="blue")
#find the 5 minutes contains the maximum 5 minutes of steps.
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
#imputing the missing values
totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissings
#fill data with mean of the day
getMeanStepsPerInterval<-function(interval){
  stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
#create data set with no missed values
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
  if(is.na(activityDataNoNA[i,]$steps)){
    activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
  }
}
#draw histograme for the new data set 
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps,col="red")
#find the mean & median of the new data set 
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
#find difference in week day and weekends
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
  if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
    activityDataNoNA[i,]$day<-"weekend"
  }
  else{
    activityDataNoNA[i,]$day<-"weekday"
  }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
#draw weekday vs weekend
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
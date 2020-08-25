Reproducible Research: Peer Assessment 1

Loading and preprocessing the data

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
# read in data. 
activityData <- read.csv("activity.csv", 
                         header = TRUE, 
                         stringsAsFactors = FALSE)

# convert date column to POSIXct
activityData$date <- ymd(activityData$date)

# create an index column for intervals (1 - 288 per day). This is the least fussy way I discovered to allow for continuous graphing without R focing lines between gaps from 55s and 1m
activityData$intNum <- 1:(24*60/5)

# filter out rows with NA values in steps
activityFilt <- activityData %>%
  filter(!is.na (steps))
What is mean total number of steps taken per day?

Calculate the total number of steps taken per day.
totalActivity <- activityFilt %>%
  select(steps, date) %>%
  group_by(date) %>%
  summarize(totalSteps = sum(steps))

head(totalActivity)
## Source: local data frame [6 x 2]
## 
##         date totalSteps
## 1 2012-10-02        126
## 2 2012-10-03      11352
## 3 2012-10-04      12116
## 4 2012-10-05      13294
## 5 2012-10-06      15420
## 6 2012-10-07      11015
Make a histogram of the total number of steps taken each day.
g <- ggplot(totalActivity, aes(x = totalSteps))

g <- g + geom_histogram(aes(y=..count../sum(..count..)), fill = "blue") +
  ggtitle("Total Steps Taken Each Day") +
  xlab("Total Steps Taken Per Day") + 
  ylab("Proportion")

g
<<<<<<< HEAD:PA1_template.md 



2d0667dfec8cef433f191834b96c0c879c79399d:PA1.md
Calculate and report the mean and median of the total number of steps taken per day.
# mean activity
meanActivity <- mean(totalActivity$totalSteps)
meanActivity
## [1] 10766.19
# median activity
medianActivity <- median(totalActivity$totalSteps)
medianActivity
## [1] 10765
What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# group by interval index (1-288)and find the average steps for that interval over all included days. 
meanSteps <- activityFilt %>%
  group_by(intNum) %>%
  mutate(meanSteps = mean(steps))

# multiply the five minute index interval (1-288) by five to correspond with five minute interval, then divide by sixty to convert to hours for a nice x axis timeline.
g <- ggplot(meanSteps, aes(x = intNum*5/60, y = meanSteps))

g + geom_line() + 
  ggtitle("Average Steps by Five Minute Interval ") + 
  xlab("Five Minute Intervals (divided into hour of the day)") + 
  ylab("Mean Number of Steps")
<<<<<<< HEAD:PA1_template.md 



2d0667dfec8cef433f191834b96c0c879c79399d:PA1.md
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
# use whichmax to find interval has the highest mean steps
meanSteps[[which.max(meanSteps$meanSteps), "interval"]]
## [1] 835
Inputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
length(is.na(activityData$steps))
## [1] 17568
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# set NAs equal to mean of steps on that day. 

activityDataComplete <- activityData %>%
  group_by(date) %>%
  transform(steps = ifelse(is.na(steps), 
                           as.integer(mean(steps, na.rm = TRUE)),
                           steps))
Create a new dataset that is equal to the original dataset but with the missing data filled in.
See number 2 above.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalActivityComplete <- activityDataComplete %>%
  select(steps, date) %>%
  group_by(date) %>%
  summarize(totalSteps = sum(steps))

g <- ggplot(totalActivityComplete, aes(x = totalSteps))
g + geom_histogram(aes(y=..count../sum(..count..))) +
  ggtitle("Total Steps Taken (NA Values Removed)") +
  xlab("Total Steps Taken") +
  ylab("Proportion")
<<<<<<< HEAD:PA1_template.md 



2d0667dfec8cef433f191834b96c0c879c79399d:PA1.md
meanActivityNoNA <- mean(totalActivityComplete$totalSteps)
medianActivitynoNA <- median(totalActivityComplete$totalSteps)

combined <- rbind(meanActivity, 
                  medianActivity, 
                  meanActivityNoNA, 
                  medianActivitynoNA)

combined
##                        [,1]
## meanActivity       10766.19
## medianActivity     10765.00
## meanActivityNoNA   10751.74
## medianActivitynoNA 10656.00
Overall, substituting NA values for the mean step value which occurred on that given day had only a slight effect on the mean and median step values. The replacement of NA values in this way produced an almost indiscernable change in the mean value, and resulted in a decrease of 100 in the median value.

Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# create a weekday factor variable. convert dates to weekdays, if the name is now "Saturday" or "Sunday", label as "Weekend", otherwise, "Weekday"
activityDataComplete$weekday <- as.factor(ifelse(weekdays(activityDataComplete$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")) 
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
# group data by the interval index, then by weekday/weekend. take the mean of that interval index (in the weekday/weekend groups)
activityDataComplete <- activityDataComplete %>% 
  group_by(intNum, weekday) %>%
  mutate(meanSteps = mean(steps))

#create plot
g <- ggplot(activityDataComplete, aes(x = intNum*5/60, y = meanSteps))
g <- g + geom_line() + xlab("Hour of the day") + 
  ggtitle("Mean Steps Taken on Weekends and Weekdays") +
  xlab("Five Minute Intervals (divided into hour of the day)") + 
  ylab("Mean Steps")

g + facet_grid(weekday ~ .)
<<<<<<< HEAD:PA1_template.md 

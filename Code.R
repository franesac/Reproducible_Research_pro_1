
##LOAD DATA
activityData <- read.csv ("activity.csv", header = T, sep = ",", stringsAsFactors = F)

##convert the date column to the appropriate format
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")

##ANALYSIS
##1. What is the mean total number of steps taken per day?
library (dplyr)
AvgDay <- activityData %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T), 
            mean.steps = mean(steps, na.rm = T))

library(ggplot2)
g <- ggplot(AvgDay, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),  
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")

##The histogram shows the largest count around the 10000-12500 step class thus we can infer that the median will be in this interval, the data is symmetrically distributed around the center of the distribution, except for one class at the extreme left.
##Let's get a summary of the data, which will include the mean and the median, to get a more quantitative insight of the data:

summary(AvgDay$total.steps)
summary (AvgDay$mean.steps)

##2. What is the daily activity pattern?

AvgInterval <- activityData %>% group_by(interval) %>%
  summarize(mean.steps = mean(steps, na.rm = T))

g <- ggplot(AvgInterval, aes(x = interval, y = mean.steps))
g + geom_line() + theme(axis.text = element_text(size = 12), 
                        axis.title = element_text(size = 14, face = "bold")) + 
  labs(y = "Mean number of steps") + labs(x = "Interval")

##We can observe the largest amount of steps occurs between time intervals 500 and 1000. The maximum average number of steps is: 206 and occurs in time interval #835

##3. Imputing missing values

## the percentage of missing data as well as the number of rows that contain an NA.
mean(is.na(activityData$steps))
sum(is.na(activityData$steps))

sum(is.na(AvgInterval$mean.steps))

newData <- activityData

for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    index <- newData$interval[i]
    value <- subset(AvgInterval, interval==index)
    newData$steps[i] <- value$mean.steps
  }
}


newAvg <- newData %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T))

g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")


summary (AvgDay$total.steps)
sd(AvgDay$total.steps, na.rm=T)
summary (newAvg$total.steps)
sd(newAvg$total.steps, na.rm=T)

#4. Are there differences in activity patterns between weekdays and weekends?

newData$day <- ifelse(weekdays(newData$date) %in% c("domingo", "sábado"), "weekend", "weekday")


wkend <- filter(newData, day == "weekend")
wkday <- filter(newData, day == "weekday")


wkend <- wkend %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
wkend$day <- "weekend"

wkday <- wkday %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
wkday$day <- "weekday"

newInterval <- rbind(wkend, wkday)
newInterval$day <- as.factor(newInterval$day)
newInterval$day <- relevel(newInterval$day, "weekend")



g <- ggplot (newInterval, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), 
                                             axis.title = element_text(size = 14)) + labs(y = "Number of Steps") + labs(x = "Interval")
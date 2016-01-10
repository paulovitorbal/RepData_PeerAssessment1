loadDepencies <- function(){
  list.of.packages <- c("stringr", "data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  for (x in list.of.packages)
    require(x, character.only = TRUE)
  remove(list.of.packages, new.packages)
}
setLocale <- function() {
  Sys.setlocale("LC_TIME", "us")  
}
loadDepencies()
setLocale()


unzip("activity.zip")
activity <- read.csv("activity.csv") 

steps.per.day <- aggregate(steps ~ date, data = activity, sum)
barplot(steps.per.day$steps, names.arg = steps.per.day$date, xlab = "", ylab = "Number of steps", las = 3)

print(paste("Mean:", mean(steps.per.day$steps),"Median is:", median(steps.per.day$steps)))


steps.per.5m.interval <- aggregate(steps ~ interval, data = activity, mean)
names(steps.per.5m.interval) <- c("5-minute interval", 
                                  "Average number of steps taken")
plot(steps.per.5m.interval, type="l")

max.steps<- which.max(steps.per.5m.interval[, 2])

print(paste("day with most steps per interval (5min):", steps.per.5m.interval[max.steps, 1]))

print(paste("Number of missing values: ", sum(!complete.cases(activity))))

avgSteps <- aggregate(steps~interval, activity[!is.na(activity$steps),], mean)
avgSteps$steps <- ceiling(avgSteps$steps)

activityCompleted <- activity
numberOfRows <- nrow(activityCompleted)
for(row in c(1:numberOfRows)){
  if (is.na(activityCompleted[row,"steps"])){
    activityCompleted[row,"steps"] <- avgSteps[avgSteps$interval == activityCompleted[row,"interval"], "steps"]
  }
}

steps.per.day1 <- aggregate(steps ~ date, data = activityCompleted, sum)
barplot(steps.per.day1$steps, names.arg = steps.per.day1$date, 
        xlab = "", ylab = "Number of steps", las = 3, width = 10)
print("The values below are considering the average for the interval for the values that are missing!")
print(paste("Mean:", mean(steps.per.day1$steps),"Median is:", median(steps.per.day1$steps)))



activity$type <- as.factor(sapply(activity$date, FUN = function(date){if (weekdays(as.Date(date)) %in% c("Saturday","Sunday")) "weekend" else "weekday"}))
par(mfrow = c(2, 1))
x <- split(activity, activity$type)
steps.weekdays <- x[[1]]
steps.weekend <- x[[2]]
steps.weekdays <- aggregate(steps ~ interval, data = steps.weekdays, mean)
steps.weekend <- aggregate(steps ~ interval, data = steps.weekend, mean)
plot(steps.weekend, type = "l", xlab = "Interval",
     ylab = "Number of steps", main = "Weekend")
plot(steps.weekdays, type = "l", xlab = "Interval",
     ylab = "Number of steps", main = "Weekday")

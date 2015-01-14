
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format="%Y-%m-%d" )

# 1-------------------------------------------------------------------------
library(ggplot2)
TotalStepsPerDay <- tapply(data$steps, data$date, FUN=sum, na.rm = TRUE)
qplot(TotalStepsPerDay, xlab="total number of steps taken each day", ylab='Frequency using binwith 1000', binwidth=1000)
MeanPerDay <- mean(TotalStepsPerDay, na.rm = TRUE)
MedianPerDay <- median(TotalStepsPerDay, na.rm = TRUE)

# 2 ---------------------------------------------------------------------------
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) + geom_line() + xlab("5-minute interval") +ylab("average number of steps taken")

averages[which.max(averages$steps),]

## 3 ------------------------------------------------------------------------
missing <- is.na(data$steps)
table(missing)      # number of missing
  
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)


## 4 ------------------------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")

library(ggplot2)
library(Hmisc)
if(!file.exists("./data")){
  dir.create("./data")
}
# 1. Load and read the data
url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,dest="./data/Factivity.zip")
unzip(zipfile = "./data/Factivity.zip",exdir = "./data")
activity<-read.csv("./data/activity.csv")


totalSteps<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
dailySteps<-as.data.frame(totalSteps)


# 2. Plotting the histogram of Total Number of Steps taken each day
dev.copy(png,file="hist.png")
ggplot(dailySteps,aes(x=totalSteps))+
  geom_histogram(binwidth=1000) +
  labs(title = "Histogram of Total Number of Steps taken each day", x = "Total Steps per day", y = "Frequency")
dev.off()

# 3. Mean and Median of Total steps per day
meanSteps <- mean(totalSteps,na.rm = TRUE)
medianSteps <- median(totalSteps,na.rm = TRUE)

meanSteps
medianSteps

# 4. Time-Series plot for average number of steps taken

dailyaverageSteps<-with(activity,tapply(steps,interval,mean,na.rm=TRUE))

dailyaverageStepsTime <- data.frame(interval=as.integer(names(dailyaverageSteps)), avgStep=dailyaverageSteps)

dev.copy(png,file="timeseries.png")
figure<-ggplot(dailyaverageStepsTime,aes(x=interval,y=avgStep))+geom_line()+ labs(title = "Time - Series Plot for Average Number of steps taken",x ="Average Steps", y ="5-minute Interval") 
print(figure)
dev.off()

# 5.Maximum Steps in the Average in the 5-minute interval
maximumSteps<-dailyaverageStepsTime[which.max(dailyaverageStepsTime$avgStep),]
maximumSteps

#6.Code to describe and show a strategy for imputing missing data
missingData<-sum(is.na(activity$steps)) 

modifiedactivity<-activity

missingRows<-is.na(modifiedactivity$steps)

modifiedactivity[missingRows,c("interval","steps")]<-dailyaverageStepsTime[as.character(modifiedactivity$interval[missingRows]),]

missingImputedData<-sum(is.na(modifiedactivity$steps)) 
missingImputedData

# 7.Histogram of the total number of steps taken each day after missing values are imputed

totalImputedSteps<-with(modifiedactivity,tapply(steps,date,sum,na.rm=TRUE))
dailyImputedSteps<-as.data.frame(totalImputedSteps)

dev.copy(png,file="imputedHist.png")
ggplot(dailyImputedSteps,aes(x=totalImputedSteps))+
  geom_histogram(binwidth=1000) +
  labs(title = "Histogram of Total Number of Steps taken per day for Modified Data", x = "Total Steps per day - Modified Data", y = "Frequency")
dev.off()

meanImputedSteps <- mean(totalImputedSteps,na.rm = TRUE)
medianImputedSteps <- median(totalImputedSteps,na.rm = TRUE)

meanImputedSteps
medianImputedSteps

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends 

#Differences in activity patterns between weekdays and weekends


modifiedactivity$date <- as.Date(modifiedactivity$date)

#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

#convert to `factor` and specify the `levels/labels`
modifiedactivity$dayType <- factor((weekdays(modifiedactivity$date) %in% weekdays1), 
                                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

# Panel plot


dailyaverageImputedSteps<-aggregate(steps~dayType +interval,modifiedactivity,FUN = mean)

dailyaverageImputedSteps <- data.frame(dailyaverageImputedSteps)

dev.copy(png,file="panel.png")
xyplot(steps~interval|dayType,dailyaverageImputedSteps,type="l" , main ="Plot of average steps taken across weekdays and weekends", xlab = "Interval", ylab = "Number of steps",layout=c(1,2))
dev.off()
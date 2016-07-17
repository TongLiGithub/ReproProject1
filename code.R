library(ggplot2)

#Download data
fileurl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, "assign.zip")
unzip("assign.zip")

#Read in data
data<-read.csv("activity.csv", header=TRUE)
head(data)
summary(data)

#Use aggregate to summarize no. of steps each day
newdata<-aggregate(data$steps, list(data$date), sum)
#An alternative (and better) way to do it: aggregate(steps ~ date, data, sum)

#Rename variables
names(newdata)[2] <- "steps"
names(newdata)[1] <- "date"

hist(newdata$steps, xlab='Steps each day', 
     main='Histogram of the total number of steps taken each day')

#Calculate mean and median number of steps taken each day
meansteps<-mean(newdata$steps, na.rm=TRUE)
mediansteps<-median(newdata$steps, na.rm=TRUE)

#Time series plot of the average number of steps taken
length(unique(data$interval))

newdata2<-aggregate(data$steps, list(data$interval), mean, na.rm=TRUE)
head(newdata2)
str(newdata2)

names(newdata2)[2] <- "steps"
names(newdata2)[1] <- "interval"

plot(newdata2$interval, newdata2$steps, xlab='Interval', 
     ylab='Ave. steps', type='l',
     main='Time series plot of the average number of steps taken')

#The 5-minute interval that, on average, contains the maximum number of steps
maxstep<-max(newdata2$steps)
newdata2$interval[which(newdata2$steps==maxstep)]

#Code to describe and show a strategy for imputing missing data
##replace NAs with interval mean of steps 
data2<-data


#Histogram of the total number of steps taken each day after missing values are 
#imputed
##Calculate NAs
sum(is.na(data$steps))
##Use ave function to replace NAs with interval mean
data2$steps[is.na(data2$steps)] <- ave(data2$steps, data2$interval, 
                FUN=function(x) mean(x,na.rm = T))[is.na(data2$steps)] 

newdata3<-aggregate(data2$steps, list(data2$date), sum)

#Rename variables
names(newdata3)[2] <- "steps"
names(newdata3)[1] <- "date"

hist(newdata3$steps, xlab='Steps each day', 
     main='Histogram of the total number of steps taken each day 
     (missing values imputed by interval means)')

#Panel plot comparing the average number of steps taken per 5-minute interval 
#across weekdays and weekends

data2$day <- weekdays(as.Date(data2$date))

data2["daytype"]<-"weekday"
                                                                                
data2$daytype[which(data2$day=="Saturday" | data2$day=="Sunday")]<-"weekend"


new<-aggregate(steps~daytype + interval, data2, mean)

ggplot(new, aes(x=interval, y=steps))+geom_line()+facet_wrap(~daytype, nrow=2)

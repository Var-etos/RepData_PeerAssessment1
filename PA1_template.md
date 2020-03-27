Reproducible Research: Peer Assessment 1
================

***27/03/2020***

Data handling
-------------

### Some code for downloading and reading the data

``` r
url1<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
down<-download.file(url = url1,"data.zip")
file1<-unzip("data.zip")
ActivityData<-read.csv(file =file1)
```

### Some code for tranforming the data

``` r
ActivityData$date<-as.Date(ActivityData$date)
```

**Steps per day**
-----------------

### Total number of steps per day

``` r
StepsPerDay<-tapply(ActivityData$steps,ActivityData$date,sum)
data.frame(StepsPerDay)
```

    ##            StepsPerDay
    ## 2012-10-01          NA
    ## 2012-10-02         126
    ## 2012-10-03       11352
    ## 2012-10-04       12116
    ## 2012-10-05       13294
    ## 2012-10-06       15420
    ## 2012-10-07       11015
    ## 2012-10-08          NA
    ## 2012-10-09       12811
    ## 2012-10-10        9900
    ## 2012-10-11       10304
    ## 2012-10-12       17382
    ## 2012-10-13       12426
    ## 2012-10-14       15098
    ## 2012-10-15       10139
    ## 2012-10-16       15084
    ## 2012-10-17       13452
    ## 2012-10-18       10056
    ## 2012-10-19       11829
    ## 2012-10-20       10395
    ## 2012-10-21        8821
    ## 2012-10-22       13460
    ## 2012-10-23        8918
    ## 2012-10-24        8355
    ## 2012-10-25        2492
    ## 2012-10-26        6778
    ## 2012-10-27       10119
    ## 2012-10-28       11458
    ## 2012-10-29        5018
    ## 2012-10-30        9819
    ## 2012-10-31       15414
    ## 2012-11-01          NA
    ## 2012-11-02       10600
    ## 2012-11-03       10571
    ## 2012-11-04          NA
    ## 2012-11-05       10439
    ## 2012-11-06        8334
    ## 2012-11-07       12883
    ## 2012-11-08        3219
    ## 2012-11-09          NA
    ## 2012-11-10          NA
    ## 2012-11-11       12608
    ## 2012-11-12       10765
    ## 2012-11-13        7336
    ## 2012-11-14          NA
    ## 2012-11-15          41
    ## 2012-11-16        5441
    ## 2012-11-17       14339
    ## 2012-11-18       15110
    ## 2012-11-19        8841
    ## 2012-11-20        4472
    ## 2012-11-21       12787
    ## 2012-11-22       20427
    ## 2012-11-23       21194
    ## 2012-11-24       14478
    ## 2012-11-25       11834
    ## 2012-11-26       11162
    ## 2012-11-27       13646
    ## 2012-11-28       10183
    ## 2012-11-29        7047
    ## 2012-11-30          NA

### Histogram of total steps taken per day

``` r
hist(StepsPerDay,ylim=c(0,40),main="Average steps per day",xlab = "Steps/day")
```

![](Figs/hist%20totalSteps%20per%20day-1.png)

### Mean & Median of total steps per day

``` r
mean(StepsPerDay,na.rm = TRUE)
```

    ## [1] 10766.19

``` r
median(StepsPerDay,na.rm = TRUE)
```

    ## [1] 10765

**Steps per interval**
----------------------

### Plot of steps per 5-min interval averaged accross all days

``` r
stepsperinterval<-ActivityData%>%
                    group_by(interval)%>%
                      summarise(steps=mean(steps,na.rm = TRUE))
plot(stepsperinterval$interval,stepsperinterval$steps,type="l",col="red",lwd=1.5,xaxt="n",xlab = "Interval Time",ylab="Steps",main = "Average steps per 5-min interval accross all days")
axis(side=1, at=seq(0,max(stepsperinterval$interval), by=100))
```

![](Figs/steps%20per%20interval%20plot-1.png)

### Interval with max no. of steps

``` r
stepsperinterval[which.max(stepsperinterval$steps),1][[1]]
```

    ## [1] 835

**Handling missing values**
---------------------------

### Total missing observations

``` r
sum(is.na(ActivityData))
```

    ## [1] 2304

### Replacing missing steps values, with its intervals mean

``` r
for (i in 1:length(ActivityData$steps)){
      if (is.na(ActivityData$steps[i]))
        ActivityData$steps[i]= stepsperinterval[match(ActivityData$interval[i],stepsperinterval$interval),2]
ActivityData$steps<-unlist(ActivityData$steps)
}
```

### New steps per day histogram

``` r
StepsPerDaynew<-tapply(ActivityData$steps,ActivityData$date,sum)
hist(StepsPerDaynew,ylim=c(0,40),main="Average steps per day with imputed values",xlab="Steps/day")
```

![](Figs/hist%20totalSteps%20per%20day%20with%20imputed%20data-1.png)

### New mean of steps per day

``` r
mean(StepsPerDaynew,na.rm = TRUE)
```

    ## [1] 10766.19

### New median of steps per day

``` r
median(StepsPerDaynew,na.rm = TRUE)
```

    ## [1] 10766.19

The new median is 10766.19 , a very small change from previous median which was 10765. The mean is the same.The impact of imputing missing data on the estimates of the total daily number of steps, is minimal in this case.

**Weekdays vs Weekends**
------------------------

### Creating a new factor variable "Day" in the dataset

``` r
ActivityData$date<-as.Date(ActivityData$date)
weekday<-weekdays(ActivityData$date)

for(i in 1:length(weekday)){
  if(weekday[i]=="Saturday"| weekday[i]=="Sunday"){
    weekday[i]<-"weekend"
  } else {
    weekday[i]<-"weekday"
   }
}   
  
ActivityData$Day<-weekday
ActivityData$Day<-as.factor(ActivityData$Day)
head(ActivityData)
```

    ##       steps       date interval     Day
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

### Plot depicting average steps for Weekend and Weekdays, per 5-minute intervals

``` r
ActivityData$interval<-as.numeric(ActivityData$interval)

StepsperWeekDay<-ActivityData%>%
  group_by(interval,Day)%>%
  summarise(steps=sum(steps))

library(ggplot2)

ggplot(StepsperWeekDay,aes(interval,steps))+
  facet_wrap(~Day,nrow=2,ncol=1)+
  scale_x_continuous(breaks=seq(0,max(StepsperWeekDay$interval),200))+
  geom_line(color="red")+
  ylab("Steps Averaged across days")+
  xlab("Interval time")+
  labs(title = "Steps per Weekday & Weekend, by interval time",size=2,
       subtitle = "Weekdays steps exceed Weekends steps")+
  theme( strip.text=element_text(size=12,color="dodgerblue4"))+
theme(plot.title=element_text(size=18, hjust=0.5, face="bold", colour="black"))+
theme(plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="chocolate"))
```

![](Figs/plot%20Weekdays%20vs%20Weekends-1.png)

Reproducible Research week-2
============================

#### Import libraries

```r
library(lubridate)
library(dplyr)
library(lattice)
```

#### Loading and preprocessing the data

```r
df<-read.csv("C:\\Education\\R programming\\data\\repdata_data_activity\\activity.csv")
```

#### What is mean total number of steps taken per day?


```r
agg<-aggregate(df$steps,by=list(df$date),FUN=sum,na.rm = TRUE)
hist(agg$x,20,xlab = "Number of steps",main="")
```

![plot of chunk simulation](figure/simulation-1.png)

```r
median(agg$x,na.rm = TRUE)
```

```
## [1] 10395
```

```r
mean(agg$x,na.rm = TRUE)
```

```
## [1] 9354.23
```

#### What is the average daily activity pattern?


```r
step_interval<-aggregate(df$steps,by=list(df$interval),FUN=mean,na.rm = TRUE)
names(step_interval)<-c("interval","steps")
plot(step_interval$interval,step_interval$steps,type="l",xlab="intervals",ylab="mean steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
step_interval$interval[which.max(step_interval$steps)]
```

```
## [1] 835
```

#### Imputing missing values



```r
table(is.na(df$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
new_df<-df
func<-function(step,interv){
    if (is.na(step)==TRUE){
        return(subset(step_interval,interval==interv)[2])
    }else{
        return(step)
    }
} 

new_df$fill_steps<-mapply(func,new_df$steps,new_df$interval)
new_df$steps<-new_df$fill_steps
new_df<-select(new_df,-(fill_steps))

new_df$steps<-as.numeric(new_df$steps)
agg2<-aggregate(new_df$steps,by=list(new_df$date),FUN=sum,na.rm=TRUE)


hist(agg2$x,20,xlab = "Number of steps",main="")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

#### Are there differences in activity patterns between weekdays and weekends?


```r
func2<-function(x){
    dat=as.Date(x,format = "%Y-%m-%d")
    day<-ifelse(wday(dat)==1 | wday(dat)==7,"weekend","weekday")
    day
}

df$day<-apply(df[2],2,func2)
agg3<-aggregate(df$steps,by=list(df$day,df$interval),mean,na.rm=TRUE)
names(agg3)<-c("day","interval","steps")
    
xyplot(steps~interval|day,data=agg3,layout=c(2,1),type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

install.packages("gridExtra")
library(rjson)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
###Setup
raw_data = fromJSON(file = "logins (3).json")
str(raw_data)
timestamps = as.POSIXlt(raw_data$login_time)
length(timestamps)
head(timestamps)
###

###Setting up buckets and binning
min(timestamps)$wday

start_time = min(timestamps)
end_time = max(timestamps)
buckets = seq.POSIXt(from = start_time, to = end_time, by = "15 min")
length(buckets)
max(timestamps) - min(timestamps)
102 * 24 * 60 / 15
hist1 = table(cut.POSIXt(timestamps, buckets))

###Converting to dataframe, adding metadata
hist1 = data.frame(hist1)
names(hist1) = c("Bucket", "Logins")
str(hist1)
#hist1$Bucket = as.POSIXct(hist1$Bucket)
hist1$Bucket = as.POSIXlt(hist1$Bucket)
hist1$Weekend = hist1$Bucket$wday %in% c(0,5, 6)
hist1$Weekday = weekdays(hist1$Bucket)

head(hist1, n= 300)


###Starting tsa
ts1 = ts(data = hist1$Logins, start = c(4*24*4,1), frequency = (4 * 24 * 7))

plot(aggregate(ts1)/(4 * 24))
ggplot(hist1, aes(Bucket, Logins, color = Weekend, group = 1), size = 1) + geom_line() + scale_x_datetime() + labs(x="Time", title="Logins per 15 Minutes")

ts1.decomp.add = decompose(ts1, type = "mult")
str(ts1.decomp.add)
ts1.decomp.add.df = data.frame(
  bucket = hist1$Bucket,
  observed = ts1.decomp.add$x, 
  seasonal = ts1.decomp.add$seasonal,
  trend = ts1.decomp.add$trend,
  random = ts1.decomp.add$random,
  weekend = hist1$Weekend)

ts1.graph.observed = ggplot(ts1.decomp.add.df, aes(bucket, observed, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime()
ts1.graph.trend = ggplot(ts1.decomp.add.df, aes(bucket, trend, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime()
ts1.graph.seasonal = ggplot(ts1.decomp.add.df, aes(bucket, seasonal, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime()
ts1.graph.random = ggplot(ts1.decomp.add.df, aes(bucket, random, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime()
grid.arrange(ts1.graph.observed, ts1.graph.trend, ts1.graph.seasonal, ts1.graph.random, ncol=1, top = "Decomposed Additive TS1")

decompose_timeseries(ts1, hist1, "mult")

plot(ts1.decomp.add)
mean(na.omit(ts1.decomp.add$random))
acf(ts1)
acf(na.omit(ts1.decomp.add$random))

ts2 = ts(data = hist1$Logins, start = c(1,4), frequency = 4 * 24 * 30)
decompose_timeseries(ts2, hist1)

ts3 = ts(data = hist1$Logins, start = c(1,4), frequency = 4)
decompose_timeseries(ts3, hist1)

ts2.decompose.add = decompose(ts2)
plot(ts2.decompose.add)
?acf
mean(na.omit(ts2.decompose.add$random))
ts1.acf = acf(na.omit(ts2.decompose.add$random), plot = FALSE)
str(ts1.acf)
ts1.acf.df = data.frame(acf = ts1.acf$acf,
                        lag = ts1.acf$lag)
ggplot(ts1.acf.df) + 
  geom_bar(aes(lag, acf), stat="identity", width = .0001, color = "black", fill = "black") + 
  geom_hline(yintercept = -1/ts1.acf$n.used + 2/sqrt(ts1.acf$n.used) * c(1, -1), color = "blue", linetype = "dashed")

plot(ts1.acf)
1 + c(-.5, .5)
pacf(na.omit(ts2.decompose.add$random))

###First differences

diffs = diff(hist1$Logins)
length(diffs) = length(hist1$Logins)
hist1 = cbind(hist1, diffs)
ts4 = ts(data = hist1$diffs, start = c(1,1), frequency = 4 * 24 * 7)
ggplot(hist1, aes(Bucket, diffs)) + geom_line() + scale_x_datetime()
decompose_timeseries(ts4, hist1)

###Looking at just one week of daily seasonal effects
ts1.decomp.add = decompose(ts1)
ts1.seasonal = data.frame(bucket=hist1$Bucket, seasonal = ts1.decomp.add$seasonal)
one_week = ts1.seasonal[0:(7*24*4),]
one_week$bucket = as.POSIXlt(one_week$bucket)
one_week$Weekday = weekdays(one_week$bucket)
one_week$Weekday = factor(one_week$Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(one_week) + 
  geom_line(aes(bucket, seasonal, color = Weekday, group=1), size=1) + 
  scale_color_brewer(palette="Set1") + 
  scale_x_datetime(breaks= date_breaks("2 hours"), minor_breaks = date_breaks("2 hour"), labels = date_format("%H", tz = "America/Toronto")) +
  labs(title="Average Daily Trends", x="Hour of the Day (24H)", y="Trend Difference")

?date_format


### Functions

plot_timeseries = function(ts){
  
}
decompose_timeseries = function(ts, histobj, type = "additive"){
  ts.decomp.add = decompose(ts, type = type)
  ts.decomp.add.df = data.frame(
    bucket = histobj$Bucket,
    observed = ts.decomp.add$x, 
    seasonal = ts.decomp.add$seasonal,
    trend = ts.decomp.add$trend,
    random = ts.decomp.add$random,
    weekend = histobj$Weekend)
  ts.graph.observed = ggplot(ts.decomp.add.df, aes(bucket, observed, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime() + labs(x="")
  ts.graph.trend = ggplot(ts.decomp.add.df, aes(bucket, trend, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime() + labs(x="")
  ts.graph.seasonal = ggplot(ts.decomp.add.df, aes(bucket, seasonal, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime() + labs(x="")
  ts.graph.random = ggplot(ts.decomp.add.df, aes(bucket, random, color = weekend, group = 1)) + geom_line(size = 1) + scale_x_datetime() + labs(x="Date, Time")
  return(grid.arrange(ts.graph.observed, ts.graph.trend, ts.graph.seasonal, ts.graph.random, ncol=1, top = "Decomposed Additive TS"))
}

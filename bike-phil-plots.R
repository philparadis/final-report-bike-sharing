source("bike-load.R")

# Aggregate data
hourly.avg.by.hour <- aggregate(.~hr, data=bike.hourly, mean)
hourly.avg.by.hour.and.mnth <- aggregate(.~hr+mnth, data=bike.hourly, mean)
hourly.avg.by.workingday <- aggregate(.~workingday, data=bike.hourly, mean)
hourly.avg.by.mnth <- aggregate(.~mnth, data=bike.hourly, mean)
hourly.avg.by.season <- aggregate(.~season, data=bike.hourly, mean)
hourly.avg.by.hour.and.season <- aggregate(.~hr+season, data=bike.hourly, mean)
daily.avg.by.workingday  <- aggregate(.~workingday, data=bike.daily, mean)
daily.avg.by.mnth <- aggregate(.~mnth, data=bike.daily, mean)
daily.avg.by.season <- aggregate(.~season, data=bike.daily, mean)
daily.avg.by.yr <- aggregate(.~yr, data=bike.daily, mean)
daily.avg.by.yr <- aggregate(.~yr, data=bike.daily, mean)

# Histogram of rentals count (daily)
hist(bike.daily$cnt)

# Histogram of rentals count (hourly)
hist(bike.hourly$cnt)

# cnt vs weather situation
plot(cnt ~ weathersit, data=bike.daily)
plot(cnt ~ weathersit, data=bike.hourly)

# plot hourly counts conditional on day of the week
hourly.avg.by.hr.and.weekday <- aggregate(.~hr+weekday, data=bike.hourly, mean)
xyplot(cnt ~ hr | weekday, data=hourly.avg.by.hr.and.weekday, type="o")

# plot hourly counts conditional on weekday AND if it's a holiday
specialdays <- aggregate(.~hr+weekday+holiday, data=bike.hourly, mean)
xyplot(cnt ~ hr | weekday+holiday, data=specialdays, type="o")

# histograms

# plot of average daily count for bike rental
plot(cnt ~ hr, data=hourly.avg.by.hour, type="o")

# xyplot of average hourly count conditional on month of the year
xyplot(cnt ~ hr | mnth, data=hourly.avg.by.hour.and.mnth)

# coplot of average daily count for bike rental conditionally on season
coplot(cnt ~ hr | season, data=hourly.avg.by.hour.and.season, type="o")

# coplot of average daily count for bike rental conditionally on season
coplot(cnt ~ hr | mnth, data=hourly.avg.by.hour.and.mnth, type="o")

# barplot average daily count for bike rental
xlabs <- barplot(hourly.avg.by.hour$cnt)
axis(side=1, at=xlabs, labels=as.character(hourly.avg.by.hour$hr))

# more barplot
#barplot(table(daily.avg.by.season[,c("season","temp","hum","casual","registered")])

        
# boxplots
boxplot(50*atemp ~ mnth, data=bike.daily)

# Plot typical week
plot(cnt ~ datetime, data=bike.hourly[1:(24*7),], type="o")

# Plot typical day
plot(cnt ~ hr, aggregate(.~hr, data=bike.hourly, mean), type="o", col="green")
points(cnt ~ hr, data=bike.hourly[(1:24)+24*100+2,], type="o")


# TODO: How can we easily visualize where the PEAK rental periods are? Not just
# on a weekly basis, but overall.



source("bike-load.R")

##################################################################
#scatterplot matrix

panel.smooth.asp <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                              cex = 1, col.smooth = "red", span = 2/3, iter = 3, asp,...) 
{
  #browser()
  points(x, y, pch = pch, col = col, bg = bg, cex = cex, asp=1)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth,...) 
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*r
  text(0.5, 0.5, txt, cex = cex.cor)
}

## put histograms on the diagonal
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

#scatterplot matrix
pairs(bike.daily[,3:15], upper.panel=panel.cor, diag.panel=panel.hist)
pairs(bike.daily[,3:15], col= bike.daily$season + 1)#, upper.panel=panel.cor, diag.panel=panel.hist)
##########################################################
# Histogram of rentals count (daily)
hist(bike.daily$cnt)

# Histogram of rentals count (hourly)
hist(bike.hourly$cnt)

# cnt vs weather situation
plot(cnt ~ weathersit, data=bike.daily)
plot(cnt ~ weathersit, data=bike.hourly)

# coplot, xyplot
coplot(casual ~ cnt|weathersit, data=bike.hourly)

#when it is weekend or holiday, less registered users
coplot(registered ~ cnt|weathersit,  data=bike.hourly)
coplot(registered ~ cnt|weathersit, col=as.integer(bike.hourly$workingday), data=bike.hourly)

coplot(casual ~ registered|cnt, data=bike.daily)
coplot(casual ~ registered|mnth,col=bike.daily$season+1, data=bike.daily)
coplot(casual ~ registered|season,col=as.numeric(bike.daily$season)+1, data=bike.daily)
# histograms

avg.by.season <- aggregate(.~season, bike.daily, mean)

# barplot

# boxplots
boxplot(50*atemp ~ mnth, data=bike.daily)
boxplot(temp ~ mnth, data=bike.daily)
boxplot(hum ~ mnth, data=bike.daily)
boxplot(windspeed~ mnth, data=bike.daily)
boxplot(casual~ mnth, data=bike.daily)
boxplot(registered~ mnth, data=bike.daily)

#################################################################################
#Phil plot

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
plot(cnt ~ hr, data=aggr.by.hour, type="o")

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
barplot(table(daily.avg.by.season[,c("season","temp","hum","casual","registered")])
        barplot(table(cars[,c("cylinders", "origin")]), mar=c(0,0,0,0),
                beside=TRUE, col=rainbow(5), ylab= "Total",
                legend.text=c("3 cylinders", "4 cylinders", "5 cylinders",
                              "6 cylinders", "8 cylinders"),
                args.legend=list(x=18.6, y=155, cex=0.85, ncol=3))        
        
        
        # boxplots
        boxplot(50*atemp ~ mnth, data=bike.daily)
        
        # Plot typical week
        plot(cnt ~ datetime, data=bike.hourly[1:(24*7),], type="o")
        
        # Plot typical day
        plot(cnt ~ hr, aggregate(.~hr, data=bike.hourly, mean), type="o", col="green")
        points(cnt ~ hr, data=bike.hourly[(1:24)+24*100+2,], type="o")
        
        
        # TODO: How can we easily visualize where the PEAK rental periods are? Not just
        # on a weekly basis, but overall.
        
        


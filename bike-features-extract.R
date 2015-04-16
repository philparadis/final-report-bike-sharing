# DESCRIPTION:
# We perform features extraction / features engineering in this file.
# Note that we only perform this on the imputated dataset "bike.hi", for
# simplicity's sake and because this is the dataset we really care about.

bike.hfx <- bike.hi

###################################
# FLAG BAD DAYS (Poor imputation)
###################################
# The following days have a significant amount of missing data:
# "2011-01-18 UTC" "2011-01-26 UTC" "2011-01-27 UTC" "2011-02-22 UTC"
# "2011-08-27 UTC" "2011-08-28 UTC" "2012-10-29 UTC" "2012-10-30 UTC"
# and it was determined that the imputation was acceptable only for
# date "2011-02-22 UTC" date "2011-08-28"

# Moreover, we found some outliers on the following days:
# "2011-04-16", "2011-10-29", "2011-12-07", "2012-04-22"

bike.hfx$badrow <- factor(rep(0, nrow(bike.daily)), levels=levels.binary)
bad.dates <- as.POSIXct(c("2011-01-18", "2011-01-26", "2011-01-27",
                          "2011-08-27", "2012-10-29", "2012-10-30",
                          "2011-04-16", "2011-10-29", "2011-12-07",
                          "2012-04-22"), tz="UTC")
bike.hfx[bike.hfx$date %in% bad.dates, "badrow"] <- factor(1, levels=levels.binary)

##################
# SPECIAL EVENTS
##################

# Christmas and Chrimsas eve: December 25-26

############
# HOLIDAYS
############

# Print the distribution of holidays during the week days
table(bike.hi[bike.hi$holiday==1, "weekday"])/24

# The result is
#  Sun  Mon  Tue  Wed Thur  Fri  Sat 
#    0   15    1    1    2    2    0

# Now it would be interesting to look at what happens on weeks where there
# is a holiday. Some questions:
# * Does a holiday on Thursday influence bikes demand on Friday?
# * Similarly, does a holiday on Tuesday influence bikes demand on Monday?
# * Does a holiday on Friday influence bikes demand over the weekend?
# * Does a holiday on Friday influence bikes demand on the next Monday?

ind.tue <- which(bike.hi$holiday==1 & bike.hi$weekday == "Tue")[[1]]
ind.wed <- which(bike.hi$holiday==1 & bike.hi$weekday == "Wed")[[1]]
ind.thur1 <- which(bike.hi$holiday==1 & bike.hi$weekday == "Thur")[[1]]
ind.thur2 <- which(bike.hi$holiday==1 & bike.hi$weekday == "Thur")[[25]]

date.tue <- bike.hi[ind.tue,"date"]
date.wed <- bike.hi[ind.wed,"date"]
date.thur1 <- bike.hi[ind.thur1,"date"]
date.thur2 <- bike.hi[ind.thur2,"date"]

for (date in list(date.tue, date.wed, date.thur1, date.thur2)) {
  bike.plot.dates(date + as.difftime(-3, unit="days"), date + as.difftime(3, unit="days"),
                  bike.hi, feature="cnt")
  bike.plot.dates(date + as.difftime(-3, unit="days"), date + as.difftime(3, unit="days"),
                  bike.hi, feature="casual", add=TRUE, col="orange")
  bike.plot.dates(date + as.difftime(-3, unit="days"), date + as.difftime(3, unit="days"),
                  bike.hi, feature="registered", add=TRUE, col="blue")
}

# Indeed, the monday before the tuesday and the fridays after the thursdays
# both look like holidays as well... It seems that people do love their long
# weekends and will take a vacation in such instances.

mon.before.tue.indices <- bike.hfx$date == date.tue + as.difftime(-1, units="days")
bike.hfx[mon.before.tue.indices,"holiday"] <- factor(1, levels=levels(bike.hfx[0,"holiday"]))
bike.hfx[mon.before.tue.indices,"workingday"] <- factor(0, levels=levels(bike.hfx[0,"workingday"]))

fri.after.thur.indices <- bike.hfx$date == date.thur1 + as.difftime(1, units="days")
bike.hfx[fri.after.thur.indices,"holiday"] <- factor(1, levels=levels(bike.hfx[0,"holiday"]))
bike.hfx[fri.after.thur.indices,"workingday"] <- factor(0, levels=levels(bike.hfx[0,"workingday"]))
fri.after.thur.indices <- bike.hfx$date == date.thur2 + as.difftime(1, units="days")
bike.hfx[fri.after.thur.indices,"holiday"] <- factor(1, levels=levels(bike.hfx[0,"holiday"]))
bike.hfx[fri.after.thur.indices,"workingday"] <- factor(0, levels=levels(bike.hfx[0,"workingday"]))

###################################################################
# TRANSFORM 'atemp' AND 'hum' INTO DIFFERENCE
# FROM "IDEAL VALUES" (INDUCING LINEAR RELATIONSHIP WITH 'cnt')
###################################################################

# Transform the temperature into a "distance from ideal temperature"
bike.hfx$atempdiff <- abs(bike.hfx$atemp - 0.7)
# Transform the humidity into a "distance from ideal humidity"
bike.hfx$humdiff <- abs(bike.hfx$hum - 0.2)

# Transform datetime to a numeric value between 0.0 and 1.0
# first.day <- bike.transformed$datetime[1]
# last.day <- tail(bike.transformed$datetime, 1)
# bike.transformed$datetime <- as.numeric(difftime(bike.transformed$datetime, first.day, units="days")) / 
#   as.numeric(difftime(last.day, first.day, units="days"))


##########################
# Simplify type of day
##########################

# To make things simpler, we introduce a new variable "typeofday" with
# only 2 levels: "work" and "relax". On any Saturday, Sunday or holiday,
# the day is of type "relax", otherwise it is of type "work".

# Even though a good classifier should be able to learn from a combination
# of "weekday" and "holiday" when is a work day versus a rest day, replacing
# "holiday" with "typeofday" should help train classifiers more efficiently.

# levels.typeofday = levels(as.factor(c("relax", "work")))
# 
# ind.tod <- bike.hfx$weekday == "Sat" | bike.hfx$weekday == "Sun" | bike.hfx$holiday == 1
# tod <- rep(factor("work", levels=levels.typeofday), nrow(bike.hfx))
# tod[ind.tod] <- factor("relax", levels=levels.typeofday)
# bike.hfx$typeofday <- tod


##########################
# Auxiliary datasets
##########################

setClass("myDateMDY")
setAs("character", "myDateMDY", function (from) as.POSIXct(strptime(from, "%m/%d/%Y", tz="UTC")))

cta.filename <- "data/CTA-ridership-L-station-entries-daily-totals.csv"
cta <- read.csv(cta.filename,
                header = TRUE,,
                colClasses = c("integer",
                               "character",
                               "myDateMDY",
                               "factor",
                               "integer"))

good.stations <- c(40190, 40820, 40890, 40930, 41050, 41060, 41420)

oldpar <- par(mfrow=c(2,1), mar=c(2,2,1,1))
for (station in good.stations) {
  station.cta <- cta[cta$station_id == station, ]
  begin <- which(station.cta$date == as.POSIXct("2011-01-01", tz="UTC"))
  end <- which(station.cta$date == as.POSIXct("2012-12-31", tz="UTC"))
  if (length(begin) == 0 || length(end) == 0) {
    cat("Can't find begin or end date...\n")
  } else {
    plot(casual ~ date, data=bike.daily)
    plot(rides ~ date, data=station.cta[begin:end,],
         main=paste(station.cta[1,"station_id"], station.cta[1,"stationname"]))
    cat ("Press [enter] to continue")
    line <- readline()
    if (line == "q")
      break
  }
}
par(oldpar)

station.40890 <- cta[cta$station_id == 40890,]
station.40930 <- cta[cta$station_id == 40930,]
station.41050 <- cta[cta$station_id == 41050,]
station.41420 <- cta[cta$station_id == 41420,]

plot(casual ~ date, data=bike.daily)

begin <- which(station.40890$date == as.POSIXct("2011-01-01", tz="UTC"))
end <- which(station.40890$date == as.POSIXct("2012-12-31", tz="UTC"))
ind <- match(unique(station.40890[begin:end, "date"]), station.40890$date)
sm <- loess.smooth(ind, station.40890[ind, "rides"], span=0.1, col="red", evaluation=length(ind))
scale <- max(sm$y)
sm$y <- sm$y/scale
plot(ind, sm$y)

bike.hfx$cta1 <- rep(sm$y, each=24)

begin <- which(station.41420$date == as.POSIXct("2011-01-01", tz="UTC"))
end <- which(station.41420$date == as.POSIXct("2012-12-31", tz="UTC"))
ind <- match(unique(station.41420[begin:end, "date"]), station.41420$date)
sm <- loess.smooth(ind, station.41420[ind, "rides"], span=0.1, col="red", evaluation=length(ind))
scale <- max(sm$y)
sm$y <- sm$y/scale
plot(ind, sm$y)

bike.hfx$cta2 <- rep(sm$y, each=24)

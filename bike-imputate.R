# DESCRIPTION:
# This files imputate all the missing data in the bike.hourly dataset.
# It returns a new dataset called: bike.hourly.imputated
# It also returns the same dataset with a shorter name: bike.hi

source("bike-load.R")

library(zoo)
library(cluster)
library(fpc)

imputate.bike.hourly.dataset <- function()
{
  ####################
  #build the matrix, each row is a day, 
  #24 attributes with counts from each hour in this day
  first.day <- bike.hourly$datetime[1]
  difftime(bike.hourly$date, first.day, units="days")+1
  
  # create a new data.frame that will contain 24 columns indicating the "cnt"
  # each row indicate a day
  bike.24hourscnt <- c()
  
  for (date in bike.daily$date) {
    # Find all "cnt" values for the matching "date"
    # Note: There might be less than 24 values since some hours are missing
    results <- bike.hourly[bike.hourly[,2] == date, c("hr","cnt")]
    # Here we are using some R black magic to handle NA values when
    # constructing the row of "cnt" over 24 hours
    counts <- rep(NA, 24)
    counts[match(results[,1],0:23)] <- 0
    counts[!is.na(counts)] <- results[,2]
    names(counts) <- paste0("hr",0:23)
    bike.24hourscnt <- rbind(bike.24hourscnt, data.frame(as.list(counts)))
  }
  rownames(bike.24hourscnt) <- 1:nrow(bike.daily)
  
  # Count the number of NA values per day
  apply(is.na(bike.24hourscnt), 1, sum)
  #inspect how many missing values in each row
  table(apply(is.na(bike.24hourscnt), 1, sum))
  #which rows have more than 5 missing values
  (bad.rows <- which(apply(is.na(bike.24hourscnt), 1, sum) > 5))
  #Print all dates with more than 5 missing values
  bike.daily[bad.rows, "date"]
  # Count how many days have more than 1 NA value
  sum(apply(is.na(bike.24hourscnt), 1, sum) > 1)
  
  #filling in the missing values by "zoo" package
  #identify the rows with 1 missing value
  #convert the data frame into a matrix
  M <- as.matrix(bike.24hourscnt)
  
  # Imputation of missing values using:
  #   locf - just use the last known value (piece-wise constant)
  #   approx - use linear interpolation between last 2 known values
  #   spline - use spline interpolation between last 2 known values
  M.locf <- t(matrix(na.locf(as.vector(t(M))), 24, 731))
  M.approx <- t(matrix(na.approx(as.vector(t(M))), 24, 731))
  M.spline <- t(matrix(na.spline(as.vector(t(M))), 24, 731))
  
  hourly.avg.by.hr.mnt.workingday <- aggregate(.~hr+mnth+workingday, data=bike.hourly, mean)
  num.bad.rows <- length(bad.rows)
  
  oldpar <- par(mfrow=c(num.bad.rows,3), mar=c(3,3,1,0), oma=c(0,0,0,0))
  for (row in bad.rows) {
    avg.counts <- c()
    for (rel in c(-1,0,1)) {
      month <- bike.daily[row+rel, "mnth"]
      workingday <- bike.daily[row+rel, "workingday"]
      indices <- hourly.avg.by.hr.mnt.workingday$mnth == month &
        hourly.avg.by.hr.mnt.workingday$workingday == workingday
      avg.counts <- c(avg.counts, hourly.avg.by.hr.mnt.workingday[indices, "cnt"])
    }
    plot(1:72, avg.counts, type="o", col="green", main=bike.daily[row,"date"])
    lines(1:72, as.vector(t(M.locf[c(row-1,row,row+1),])),type='o',col="red")
    lines(1:72, as.vector(t(M[c(row-1,row,row+1),])),type='o')
    plot(1:72, avg.counts, type="o", col="green")
    lines(1:72, as.vector(t(M.approx[c(row-1,row,row+1),])),type='o',col="red")
    lines(1:72, as.vector(t(M[c(row-1,row,row+1),])),type='o')
    plot(1:72, avg.counts, type="o", col="green")
    lines(1:72, as.vector(t(M.spline[c(row-1,row,row+1),])),type='o',col="red")
    lines(1:72, as.vector(t(M[c(row-1,row,row+1),])),type='o')
  }
  par(oldpar)
  
  # According to the graphs above, we have to conclude that all the bad rows
  # with 6 or more missing values are "too bad" and should be discarded completely,
  # except the values with index 27 (date 2011-02-22) and the value with index 240
  # (date 2011-08-28)
  
  # Moreover, it appears that imputation of all other missing values worked fine
  # and that the "approx" method is sufficient and always gives non-negative results.
  
  bad.rows <- bad.rows[bad.rows != 27 & bad.rows != 240]
  (bad.rows)
  
  datetime.imputated <- first.day + seq(0, (length(M.approx)-1)*3600, 3600)
  cnt.imputated <- as.vector(t(M.approx))
  
  
  # Can we imputate all the other features?
  
  first.day <- bike.hourly$datetime[1]
  
  difftime(bike.hourly$date, first.day, units="days")+1
  
  
  # create a new data.frame that will contain 24 columns indicating the "cnt"
  # each row indicate a day
  
  newFeaturesContinuousVars <- NULL
  features.to.imputate <- c("atemp", "temp", "hum", "windspeed", "casual", "registered")
  for (feature in names(bike.hourly[,features.to.imputate])) {
    hr.names <- paste0("hr",0:23)
    TMP <- c()
    for (date in bike.daily$date) {
      results <- bike.hourly[bike.hourly[,2] == date, c("hr",feature)]
      counts <- rep(NA, 24)
      counts[match(results[,1],0:23)] <- 0
      counts[!is.na(counts)] <- results[,2]
      names(counts) <- hr.names
      TMP <- rbind(TMP, data.frame(as.list(counts)))
    }
    C <- as.vector(t(TMP)) # Write as a column
    D <- na.approx(C)
    newFeature <- setNames(data.frame(D), feature)
    if (is.null(newFeaturesContinuousVars))
      newFeaturesContinuousVars <- newFeature
    else
      newFeaturesContinuousVars <- cbind(newFeaturesContinuousVars, newFeature)
  }
  
  # Imputate season, yr, mnth, hr, weekday by calculating their value
  # from "datetime.imputated"
  get.seasons <- function(month, day)
  {
    ((month == 12 & day >= 21) | month < 3 | (month == 3 & day < 21) )*1 +
      ((month == 3 & day >= 21) | (3 < month & month < 6) | (month == 6 & day < 21))*2 +
      ((month == 6 & day >= 21) | (6 < month & month < 9) | (month == 9 & day < 22))*3 +
      ((month == 9 & day >= 22) | (9 < month & month < 12) | (month == 12 & day < 21))*4
  }
  
  date.imputated <- strptime(datetime.imputated, "%Y-%m-%d", tz="UTC")
  yr.imputated <- factor(as.POSIXlt(datetime.imputated)$year+1900-2011, levels=c(0,1),
                         labels=c("2011","2012"))
  mnth.imputated <- factor(as.POSIXlt(datetime.imputated)$mon+1,
                           levels=1:12,
                           labels=c("Jan","Feb","Mar","Apr",
                                    "May","Jun","Jul","Aug",
                                    "Sep","Oct","Nov","Dec"))
  day.imputated <- as.POSIXlt(datetime.imputated)$mday
  season.imputated  <- factor(get.seasons(as.numeric(mnth.imputated), day.imputated),
                              levels=c(1,2,3,4),
                              labels=c("spring","summer","fall","winter"))
  hr.imputated <- factor(as.POSIXlt(datetime.imputated)$hour)
  weekday.imputated <- factor(as.POSIXlt(datetime.imputated)$wday, levels=0:6,
                              labels=c("Sun","Mon","Tue","Wed","Thur","Fri","Sat"))
  
  # Imputate 'holiday', 'weekday', 'workingday' and 'weathersit' by taking a
  # majority vote on the existing values in the same day... Not completely
  # accurate for 'weathersit', but should be good enough.
  newFeaturesMajorityVote <- NULL
  features.to.imputate <- c("holiday", "weekday", "workingday", "weathersit")
  for (feature in names(bike.hourly[, features.to.imputate])) {
    hr.names <- paste0("hr",0:23)
    TMP <- NULL
    empty.counts <- factor(rep(NA, 24), levels=levels(bike.hourly[0,feature]))
    for (date in bike.daily$date) {
      results <- bike.hourly[bike.hourly[,2] == date, c("hr",feature)]
      counts <- empty.counts
      counts[match(results[,1],0:23)] <- results[,2]
      names(counts) <- hr.names
      if (is.null(TMP))
        TMP <- data.frame(lapply(counts, function(x) t(data.frame(x))))
      else
        TMP <- rbind(TMP, data.frame(lapply(counts, function(x) t(data.frame(x)))))
    }
    # Check to see if any row is all NAs
    test.empty.rows <- apply(TMP, 1, function(x) all(is.na(x)))
    if (any(test.empty.rows)) {
      stop("Too much missing data. Can't apply imputation method if there is no data for a complete day.")
    }
    # Calculate majority vote for each row
    D <- t(apply(TMP, 1, function (x) {x[is.na(x)] <- names(which.max(table(x))); x}))
    E <- as.vector(t(D)) # Write as a column
    newFeature <- setNames(data.frame(E), feature)
    if (is.null(newFeaturesMajorityVote))
      newFeaturesMajorityVote <- newFeature
    else
      newFeaturesMajorityVote <- cbind(newFeaturesMajorityVote, newFeature)
  }
  
  # Finally, construct the full imputated dataset
  bike.hourly.imputated <- cbind(data.frame(instant = 1:(nrow(bike.daily)*24),
                                            date = date.imputated,
                                            datetime = datetime.imputated,
                                            season = season.imputated,
                                            yr = yr.imputated,
                                            mnth = mnth.imputated,
                                            hr = hr.imputated),
                                 with(newFeaturesMajorityVote,
                                      data.frame(holiday = holiday,
                                                 weekday = weekday,
                                                 workingday = workingday,
                                                 weathersit = weathersit)),
#                                       data.frame(holiday = factor(holiday, levels=1:2, labels=c("0","1")),
#                                                  weekday = factor(weekday, levels=1:7,
#                                                                              labels=c("Sun","Mon","Tue","Wed","Thur","Fri","Sat")),
#                                                  workingday = factor(workingday, levels=1:2, labels=c("0","1")),
#                                                  weathersit = factor(weathersit, levels=c(1,2,3,4),
#                                                                                 labels=c("clear","misty","rainy","stormy")))),
                                 with(newFeaturesContinuousVars,
                                      data.frame(atemp = atemp,
                                                 temp = temp,
                                                 hum = hum,
                                                 windspeed = windspeed,
                                                 casual = casual,
                                                 registered = registered)),
                                 data.frame(cnt = cnt.imputated))
  bike.hourly.imputated
}

bike.hourly.imputated <- NULL
bike.hi <- NULL

if (!exists("bike.hourly.imputated") || is.null(bike.hourly.imputated)) {
  bike.hourly.imputated <<- imputate.bike.hourly.dataset()
  # Shorter name for the above dataset
  bike.hi <<- bike.hourly.imputated
}
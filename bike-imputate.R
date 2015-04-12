source("bike-load.R")

library(zoo)
library(cluster)
library(fpc)

tmp <- bike.hourly[1:48,]
plot(cnt ~ datetime, data=tmp, type="o")

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
apply(is.na(bike.24hourscnt), 1, sum)
#which rows have more than 5 missing values
(bad.rows <- which(apply(is.na(bike.24hourscnt), 1, sum) > 5))
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

# We are left with 725 days
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

# Count how many days have more than 1 NA value
sum(apply(is.na(bike.24hourscnt), 1, sum) > 1)
#head(bike.24hourscnt)
#bike.24hourscnt[2,]

#filling in the missing values by "zoo" package
#identify the rows with 1 missing value
#convert the data frame into a matrix
M <- as.matrix(bike.24hourscnt)

#filling method 1: fill the missing value by last observation 
apply(is.na(bike.24hourscnt), 1, sum) == 1
#filling second row as an example
#found second row has 1 missing value, use as an example
#plot show missing value at hr5
M[2,]
plot(1:24, M[2,],type='o')
(na.locf(M[2,]))
plot(1:24, na.locf(M[2,]),type='o')
#fill the whole matrix
# A <- matrix(unname(unlist(apply(M, 1, na.locf, na.rm=FALSE))), 731, 24)
# B <- matrix(unname(unlist(apply(A, 1, na.locf, na.rm=FALSE, fromLast=TRUE))), 731, 24)
# sum(apply(is.na(A), 1, sum) >0)
# sum(apply(is.na(B), 1, sum) >0)

A <- t(na.locf(t(M)))
B <- t(na.locf(t(A), fromLast=TRUE))

# #filling method 2:uses linear interpolation to fill in missing data
# M[2,]
# (na.approx(M[2,]))
# plot(1:24, na.approx(M[2,]),type='o')
# 
# C <- matrix(unname(unlist(apply(A, 1, na.approx, na.rm=FALSE))), 731, 24)
# sum(apply(is.na(C), 1, sum) >0)
# apply(is.na(C), 1, sum) >0
# D <- matrix(unname(unlist(apply(C, 1, na.approx, na.rm=FALSE))), 731, 24)
# apply(is.na(D), 1, sum) >0
# 
# #filling method 3: uses polynomial interpolation to fill in missing data
# M[2,]
# (na.spline(M[2,]))
# E <- matrix(unname(unlist(apply(A, 1, na.spline, na.rm=FALSE))), 731, 24)
# sum(apply(is.na(E), 1, sum) >0)
# F <- matrix(unname(unlist(apply(E, 1, na.spline, na.rm=FALSE))), 731, 24)
# sum(apply(is.na(F), 1, sum) >0)
# 
# #draw a graph with different filling methods
# plot(na.locf(M[40,]),type='o', col="red") 
# lines(na.approx(M[40,]),type='o', col="green")
# lines(na.spline(M[40,]), type='o', col="blue")
# lines(M[40,], type='o')


print.cluster.working.days <- function (data, cluster.indices, num.clusters)
{
  for (i in 1:num.clusters) {
    cl.data <- data[cluster.indices==i,]
    work.days <- sum(cl.data[,"workingday"]==1)
    total.days <- nrow(cl.data)
    cat(paste0("Cluster #", i, "\n"))
    cat(paste0("Working days: ", work.days, "\n"))
    cat(paste0("total days: ", total.days, "\n"))
    cat(paste0("percentage: ", work.days/total.days*100, "\n"))
  }
}
print.cluster.avg.temp <- function (data, cluster.indices, num.clusters)
{
  for (i in 1:num.clusters) {
    cl.data <- data[cluster.indices==i,]
    avg.tmp <- mean(cl.data[,"atemp"])
    total.days <- nrow(cl.data)
    cat(paste0("Cluster #", i, "\n"))
    cat(paste0("avg temperature: ", avg.tmp, "\n"))
    cat(paste0("total days: ", total.days, "\n"))
  }
}

#applying k-means after dealing with missing values
#kmeans on first filling: Last Observation Carried Forward
num.clusters <- 2
km1<-kmeans(B, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 3
km1<-kmeans(B, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 4
km1<-kmeans(B, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 5
km1<-kmeans(B, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 6
km1<-kmeans(B, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
print.cluster.avg.temp(bike.daily, km1$cluster, num.clusters)

# plotcluster(B, km1$cluster, col=B.all$workingday)
# clusplot(B, km1$cluster, color=TRUE, shade=TRUE, 
#         labels=2, lines=0)
#pairs(B[1:4,], col = km1$cluster)
#cluster.stats(B, fit1$cluster, fit2$cluster)

source("bike-load.R")

library(zoo)
library(cluster)
library(fpc)
library(Metrics)
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

M <- as.matrix(bike.24hourscnt)
head(M)

#plot a few days of the data
for (i in 1:10){
  plot(M[i,], type='o', xlab="hours", ylab="counts", main=paste("day", i)) 
} 

# Count the number of NA values per day
apply(is.na(bike.24hourscnt), 1, sum)

#inspect how many missing values in each row
apply(is.na(bike.24hourscnt), 1, sum)

# Count how many days have more than 1 NA value
sum(apply(is.na(bike.24hourscnt), 1, sum) > 0)
#head(bike.24hourscnt)
#bike.24hourscnt[2,]

#########filling in the missing values by "zoo" package#################
#convert the data frame into a matrix
M <- as.matrix(bike.24hourscnt)

############filling method 1: fill the missing value by last observation 
apply(is.na(bike.24hourscnt), 1, sum) == 1
#filling second row as an example
#found second row has 1 missing value, use as an example
#plot show missing value at hr5
M[2,]
(na.locf(M[2,]))
plot(1:24, na.locf(M[3,]),type='o', col="green")
lines(1:24, M[3,],type='o')
#fill the whole matrix
#A <- t(na.locf(t(M)))
#B <- t(na.locf(t(A), fromLast=TRUE))

############filling method 2:uses linear interpolation to fill in missing data
 M[2,]
 (na.approx(M[2,]))
 plot(1:24, na.approx(M[2,]),type='o')
# 


#############filling method 3: uses polynomial interpolation to fill in missing data
 M[2,]
round((na.spline(M[2,])),digits=2)


##plot examples for filling with 3 methods
test1<-M[1,]
plot(test1,type="o")
test1[c(3,5,6,10,11,15,16)]<-NA
lines(test1,type="o")
lines(na.locf(test1),type='o', col="red")
lines(na.approx(test1),type='o', col="green")
lines(na.spline(test1), type='o', col="blue")
lines(test1, type='o')
legend( x="topleft", 
        legend=c("Original","Last Observation Carried Forward","Linear Interpolation", "spline interpolation"),
        col=c("black","red","green","blue"), lwd=1, lty=c(1,1,1,1), 
        cex=0.5)
rmse(M[1,], na.locf(test1))
rmse(M[1,], na.approx(test1))
rmse(M[1,], na.spline(test1))

test2<-M[10,]
plot(test2,type="o")
test2[c(3,5,6,10,11,15,16)]<-NA
lines(na.locf(test2),type='o', col="red")
lines(na.approx(test2),type='o', col="green")
lines(na.spline(test2), type='o', col="blue")
lines(test2, type='o')
legend( x="topright", 
        legend=c("Original","Last Observation Carried Forward","Linear Interpolation", "spline interpolation"),
        col=c("black","red","green","blue"), lwd=1, lty=c(1,1,1,1), 
        cex=0.5)
rmse(M[10,], na.locf(test2))
rmse(M[10,], na.approx(test2))
rmse(M[10,], na.spline(test2))

####use combination of linear and spline intepolation to fill the whole matrix
A <- t(na.spline(t(M)))
M.approx <- t(na.approx(t(A)))
#check missing values
sum(apply(is.na(M.approx), 1, sum) >0)

####################clustering
print.cluster.working.days <- function (data, cluster.indices, num.clusters)
{
  for (i in 1:num.clusters) {
    cl.data <- data[cluster.indices==i,]
    work.days <- sum(cl.data[,"workingday"]==1)
    total.days <- nrow(cl.data)
    cat(paste0("Cluster #", i, "\n"))
    cat(paste0("Working days: ", work.days, " (",signif(work.days/total.days*100, 3),"%)\n"))
    cat(paste0("Total days: ", total.days, "\n"))
  }
}
print.cluster.holiday <- function (data, cluster.indices, num.clusters)
{
  for (i in 1:num.clusters) {
    cl.data <- data[cluster.indices==i,]
    holiday.days <- sum(cl.data[,"holiday"]==1)
    weekend.days <- sum(cl.data[,"weekday"]=="Sat" | cl.data[,"weekday"]=="Sun")
    total.days <- nrow(cl.data)
    cat(paste0("Cluster #", i, "\n"))
    cat(paste0("Holiday days: ", holiday.days, " (",signif(holiday.days/total.days*100, 3),"%)\n"))
    cat(paste0("Weekend days: ", weekend.days, " (",signif(weekend.days/total.days*100, 3),"%)\n"))
    cat(paste0("Total days: ", total.days, "\n"))
  }
}
print.cluster.avg.temp <- function (data, cluster.indices, num.clusters)
{
  for (i in 1:num.clusters) {
    cl.data <- data[cluster.indices==i,]
    avg.tmp <- mean(cl.data[,"atemp"])
    total.days <- nrow(cl.data)
    cat(paste0("Cluster #", i, "\n"))
    cat(paste0("Avg temperature: ", signif(avg.tmp, 4), "\n"))
    cat(paste0("Total days: ", total.days, "\n"))
  }
}


################applying k-means after dealing with missing values
M.approx.all<-cbind(M.approx, bike.daily)
par(mfrow=c(2, 4))
for (i in 2:9){
  km1<-kmeans(M.approx, i, 300)
  print.cluster.working.days(bike.daily, km1$cluster, i)
  print.cluster.avg.temp(bike.daily, km1$cluster, i)
  print.cluster.holiday(bike.daily, km1$cluster, i)
  plotcluster(M.approx, km1$cluster, col=M.approx.all$workingday, main=paste(i,"clusters"))
}

num.clusters <- 4
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
print.cluster.avg.temp(bike.daily, km1$cluster, num.clusters)
print.cluster.holiday(bike.daily, km1$cluster, num.clusters)
par(mfrow=c(2, 2));
for (i in 1:4) {
  plot(0:23, colMeans(M.approx[km1$cluster==i,]), main=paste0("Cluster ",i), type="o")
}

###############DBI
Davies.Bouldin <- function(A, SS, m) {
  # A  - the centres of the clusters
  # SS - the within sum of squares
  # m  - the sizes of the clusters
  N <- nrow(A)   # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i] + S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

#pdf("figures/dbi.png", width=8, height=6)
#draw clustering with DBI
oldpar <- par(mfrow = c(4, 4))
par(mar = c(2, 1, 2, 1))
errs <- rep(0, 10)
DBI <- rep(0, 10)
#
#set.seed(0)
for (i in 2:15) {
  KM <- kmeans(M.approx, i, 100)
  plotcluster(M.approx, km1$cluster, col = KM$cluster, main=paste(i,"clusters"))
  #plot(M.approx, col = KM$cluster, pch = KM$cluster,
       #main = paste(i," clusters"))
  errs[i-1] <- sum(KM$withinss)
  DBI[i-1] <- Davies.Bouldin(KM$centers, KM$withinss, KM$size)
  print(KM$centers)
}
#
plot(2:15, errs, main = "SS")
lines(2:15, errs)
#
plot(2:15, DBI, main = "Davies-Bouldin")
lines(2:15, DBI)
par(oldpar)


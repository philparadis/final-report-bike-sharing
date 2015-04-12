source("bike-imputate.R")

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

#applying k-means after dealing with missing values
#kmeans on first filling: Last Observation Carried Forward
num.clusters <- 2
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 3
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 4
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 5
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
num.clusters <- 6
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
print.cluster.avg.temp(bike.daily, km1$cluster, num.clusters)

num.clusters <- 4
km1<-kmeans(M.approx, num.clusters, 300)
print.cluster.working.days(bike.daily, km1$cluster, num.clusters)
print.cluster.avg.temp(bike.daily, km1$cluster, num.clusters)
print.cluster.holiday(bike.daily, km1$cluster, num.clusters)
par(mfrow=c(2, 2));
for (i in 1:4) {
  plot(0:23, colMeans(M.approx[km1$cluster==i,]), main=paste0("Cluster ",i), type="o")
}

# plotcluster(B, km1$cluster, col=B.all$workingday)
# clusplot(B, km1$cluster, color=TRUE, shade=TRUE, 
#         labels=2, lines=0)
#pairs(B[1:4,], col = km1$cluster)
#cluster.stats(B, fit1$cluster, fit2$cluster)


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

pdf("figures/dbi.png", width=8, height=6)
#
oldpar <- par(mfrow = c(4, 4))
par(mar = c(2, 1, 2, 1))
errs <- rep(0, 10)
DBI <- rep(0, 10)
#
set.seed(0)
for (i in 2:15) {
  KM <- kmeans(M.approx, i, 15)
  plot(M.approx, col = KM$cluster, pch = KM$cluster,
       main = paste(i," clusters"))
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


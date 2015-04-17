# DESCRIPTION:
# In this file, we plot casual, registered and cnt data for ALL days,
# no more no less :)

date.first <- bike.hi[1,"date"]
date.last <- tail(bike.hi[, "date"], 1)


pdf("figures/all-days.pdf", width=8, height=6)
oldpar <- par(mfrow=c(4,1), mar=c(2.1,4.1,2,2.1))
date <- date.first
while(date < date.last) {
  date.begin <- date
  date.end <- date + as.difftime(6, unit="days")
  if (date.end > date.last)
    date.end <- date.last
  bike.plot.dates(date.begin, date.end, bike.hi, feature="cnt")
  bike.plot.dates(date.begin, date.end, bike.hi, feature="casual", add=TRUE, col="orange")
  bike.plot.dates(date.begin, date.end, bike.hi, feature="registered", add=TRUE, col="blue")
  date <- date.end + as.difftime(1, unit="days")
}
par(oldpar)
dev.off()

pdf("figures/all-days-casual.pdf", width=8, height=6)
oldpar <- par(mfrow=c(4,1), mar=c(2.1,4.1,2,2.1))
date <- date.first
while(date < date.last) {
  date.begin <- date
  date.end <- date + as.difftime(6, unit="days")
  if (date.end > date.last)
    date.end <- date.last
  bike.plot.dates(date.begin, date.end, bike.hi, feature="casual", col="orange")
  date <- date.end + as.difftime(1, unit="days")
}
par(oldpar)
dev.off()

pdf("figures/all-days-casual-cta.pdf", width=8, height=6)
oldpar <- par(mfrow=c(4,1), mar=c(2.1,4.1,2,2.1))
date <- date.first
bike.tmp <- bike.hfx
bike.tmp$cta <- bike.tmp$cta / 12.0
while(date < date.last) {
  date.begin <- date
  date.end <- date + as.difftime(6, unit="days")
  if (date.end > date.last)
    date.end <- date.last
  bike.plot.dates(date.begin, date.end, bike.hi, feature="casual", col="orange")
  bike.plot.dates(date.begin, date.end, bike.tmp, feature="cta", col="pink", add=TRUE)
  date <- date.end + as.difftime(1, unit="days")
}
par(oldpar)
dev.off()

pdf("figures/all-days-registered.pdf", width=8, height=6)
oldpar <- par(mfrow=c(4,1), mar=c(2.1,4.1,2,2.1))
date <- date.first
while(date < date.last) {
  date.begin <- date
  date.end <- date + as.difftime(6, unit="days")
  if (date.end > date.last)
    date.end <- date.last
  bike.plot.dates(date.begin, date.end, bike.hi, feature="registered", col="blue")
  date <- date.end + as.difftime(1, unit="days")
}
par(oldpar)
dev.off()
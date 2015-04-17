source("bike-imputation.R")

augment.bike.hourly <- function (add.prev.hours)
{
  cnt.hours.ago <- c()
  
  for (x in 1:total.prev.hours) {
    # Find the indices which match the "datetime" field for to "x" hours ago
    x.hour.ago.indices <- match(bike.hourly.imputated$datetime,
                                bike.hourly.imputated$datetime + 3600 * x)
    x.hour.ago.cnt <- bike.hourly.imputated[x.hour.ago.indices, "cnt"]
    cnt.hours.ago <- cbind(cnt.hours.ago, x.hour.ago.cnt)
  }
  
  prev.hours <- cbind(data.frame(datetime=bike.hourly.imputated$datetime), cnt.hours.ago)
  colnames(prev.hours) <- c("datetime", paste0("cnt.", 1:total.prev.hours, "hr.ago"))
  
  bike.hourly.augmented <- with(bike.hourly,
                                cbind(data.frame(instant=instant,
                                                 date=date,
                                                 datetime=datetime,
                                                 season=season,
                                                 yr=yr,
                                                 mnth=mnth,
                                                 hr=hr,
                                                 holiday=holiday,
                                                 weekday=weekday,
                                                 workingday=workingday,
                                                 weathersit=weathersit,
                                                 atemp=atemp,
                                                 temp=temp,
                                                 hum=hum,
                                                 windspeed=windspeed),
                                      prev.hours[prev.hours$datetime %in% bike.hourly$datetime,],
                                      data.frame(cnt=cnt)))
  
  # Drop the first 'total.prev.hours' hours, since they contain missing values
  bike.hourly.augmented <- bike.hourly.augmented[-(1:total.prev.hours),]
  bike.hourly.augmented
}


total.prev.hours <- 24
bike.hourly.augmented <- augment.bike.hourly(total.prev.hours)
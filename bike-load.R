default.work.dir <- "D:/stat5703w/bike-sharing"
hedos.work.dir <- "/proj/stat5703w/bike-sharing"
yue.work.dir <- "C:\Users\Yue\Desktop\master's\2015 Winter\Data mining\bike-sharing"

switch(Sys.info()[['user']],
       # Working directory for user "hedos"
       hedos = {  work.dir <- hedos.work.dir },
       # Working directory for user "yue"
       Yue = { work.dir <- yue.work.dir },
       # If no matching username was found, use default working directory
       { work.dir <- default.work.dir })
setwd(work.dir)

Create 'figures' and 'objects' subdirectories if they don't exist
setwd(work.dir)
dir.create("figures", showWarnings = FALSE)
dir.create("objects", showWarnings = FALSE)

# Load relevant libraries
library(caret)
library(neuralnet)
library(randomForest)

# =========================================
#    Dataset characteristics
# =========================================   
#    Both hour.csv and day.csv have the following fields, except hr which is not available in day.csv
# 
# - instant: record index
# - dteday : date
# - season : season (1:springer, 2:summer, 3:fall, 4:winter)
# - yr : year (0: 2011, 1:2012)
# - mnth : month ( 1 to 12)
# - hr : hour (0 to 23)
# - holiday : weather day is holiday or not (extracted from http://dchr.dc.gov/page/holiday-schedule)
# - weekday : day of the week
# - workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
# + weathersit : 
# - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
# - temp : Normalized temperature in Celsius. The values are divided to 41 (max)
# - atemp: Normalized feeling temperature in Celsius. The values are divided to 50 (max)
# - hum: Normalized humidity. The values are divided to 100 (max)
# - windspeed: Normalized wind speed. The values are divided to 67 (max)
# - casual: count of casual users
# - registered: count of registered users
# - cnt: count of total rental bikes including both casual and registered

# Helper function to read and convert a date to POSIXlt type
as.myDate <- function(date)
{
   strptime(paste(date, time), format="%m/%d/%y", tz="UTC")
}
setClass("myDate")
setAs("character", "myDate", function (from) strptime(from, "%Y-%m-%d", tz="UTC"))

### Read the Bike Sharing Dataset
hourly.filename <- "data/hour.csv"
bike.hourly.raw <-read.table(hourly.filename, header=TRUE, sep=",",
                        colClasses = c("integer",
                                       "myDate",
                                       "integer",
                                       "integer",
                                       "integer",
                                       "integer",
                                       "integer",
                                       "integer",
                                       "integer",
                                       "integer",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "integer",
                                       "integer",
                                       "integer"))

### Validate that data was loaded correctly
dim(bike.hourly.raw)  # Print dimensions
bike.hourly.raw[1:5,]  # Print first 5 rows...
tail(bike.hourly.raw, 5)  # Print last 5 rows...

### Read the Bike Sharing Dataset
daily.filename <- "data/day.csv"
bike.daily.raw <-read.table(daily.filename, header=TRUE, sep=",",
                             colClasses = c("integer",
                                            "myDate",
                                            "integer",
                                            "integer",
                                            "integer",
                                            "integer",
                                            "integer",
                                            "integer",
                                            "integer",
                                            "numeric",
                                            "numeric",
                                            "numeric",
                                            "numeric",
                                            "integer",
                                            "integer",
                                            "integer"))

### Validate that data was loaded correctly
dim(bike.daily.raw)  # Print dimensions
bike.daily.raw[1:5,]  # Print first 5 rows...
tail(bike.daily.raw, 5)  # Print last 5 rows...

#############################################
# Data pre-processing
#############################################

levels.binary <- as.factor(c(0, 1))

bike.hourly <- with(bike.hourly.raw,
                    data.frame(instant=instant,
                               date=dteday,
                               datetime=dteday + hr*3600,
                               season=factor(season, levels=c(1,2,3,4),
                                             labels=c("spring","summer","fall","winter")),
                               yr=factor(yr, levels=c(0,1), labels=c("2011","2012")),
                               mnth=factor(mnth, levels=1:12,
                                           labels=c("Jan","Feb","Mar","Apr",
                                                    "May","Jun","Jul","Aug",
                                                    "Sep","Oct","Nov","Dec")),
                               hr=hr,
                               holiday=factor(holiday, levels=levels.binary),
                               weekday=factor(weekday, levels=0:6,
                                              labels=c("Sun","Mon","Tue","Wed","Thur","Fri","Sat")),
                               workingday=factor(workingday, levels=levels.binary),
                               weathersit=factor(weathersit, levels=c(1,2,3,4),
                                                 labels=c("clear","misty","rainy","stormy")),
                               atemp=atemp,
                               temp=temp,
                               hum=hum, 
                               windspeed=windspeed,
                               casual=casual,
                               registered=registered,
                               cnt=cnt))


bike.daily <- with(bike.daily.raw,
                    data.frame(instant=instant,
                               date=dteday,
                               season=factor(season, levels=c(1,2,3,4),
                                             labels=c("spring","summer","fall","winter")),
                               yr=factor(yr, levels=c(0,1), labels=c("2011","2012")),
                               mnth=factor(mnth, levels=1:12,
                                           labels=c("Jan","Feb","Mar","Apr",
                                                    "May","Jun","Jul","Aug",
                                                    "Sep","Oct","Nov","Dec")),
                               holiday=factor(holiday, levels=levels.binary),
                               weekday=factor(weekday, levels=0:6,
                                              labels=c("Sun","Mon","Tue","Wed","Thur","Fri","Sat")),
                               workingday=factor(workingday, levels=levels.binary),
                               weathersit=factor(weathersit, levels=c(1,2,3,4),
                                                 labels=c("clear","misty","rainy","stormy")),
                               atemp=atemp,
                               temp=temp,
                               hum=hum, 
                               windspeed=windspeed,
                               casual=casual,
                               registered=registered,
                               cnt=cnt))

###############################################################
# Data-preprocessing II
#  - Convert categorical variables into binary variables
#    (Split any categorical variable with more than 2 classes
#     into multiple binary variables)
#  - Also, we transform "hr" into a categorical variable first.
###############################################################

bike.hourly.tmp <- bike.hourly
bike.hourly.tmp$hr <- factor(bike.hourly.tmp$hr, levels=0:23, labels=as.character(0:23))
bike.hourly.binarized <- with(bike.hourly.tmp,
                             cbind(data.frame(instant=instant,
                                              date=date,
                                              datetime=datetime),
                                   model.matrix(~ season + 0), 
                                   model.matrix(~ yr + 0),
                                   model.matrix(~ mnth + 0),
                                   model.matrix(~ hr + 0),
                                   data.frame(holiday=as.numeric(levels(holiday))[holiday]),
                                   model.matrix(~ weekday + 0),
                                   data.frame(workingday=as.numeric(levels(workingday))[workingday]),
                                   model.matrix(~ weathersit + 0),
                                   data.frame(temp=temp,
                                              atemp=atemp,
                                              hum=hum,
                                              windspeed=windspeed,
                                              casual=casual,
                                              registered=registered,
                                              cnt=cnt)))



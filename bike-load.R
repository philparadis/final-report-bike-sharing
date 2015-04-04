default.work.dir <- "D:/stat5703w/bike-sharing"
hedos.work.dir <- "/proj/stat5703w/bike-sharing"
yue.work.dir <- "C:/Users/Yue/Desktop/STAT5703/bike-sharing"

switch(Sys.info()[['user']],
       # Working directory for user "hedos"
       hedos = {  work.dir <- hedos.work.dir },
       # Working directory for user "yue"
       Yue = { work.dir <- yue.work.dir },
       # If no matching username was found, use default working directory
       { work.dir <- default.work.dir })
setwd(work.dir)

# Create 'figures' and 'objects' subdirectories if they don't exist
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
filename <- "data/hour.csv"
d.bike <-read.table(filename, header=TRUE, sep=",",
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
# Print dimensions
dim(d.bike)

# Print first 5 rows...
d.bike[1:5,]

# Print last 5 rows...
tail(d.bike, 5)


### Example of training a classifier for regression on "cnt"

# For simplicity of using the classifier, remove column "instant", 
# "dteday" as well as all categorical variables with more than 2
# categories. Also remove "casual" and "registered" since their sum
# gives "cnt".
d.bike.easy <- d.bike[,-c(1,2,3,8, 10, 15,16)]

train.index <- 1:(nrow(d.bike.easy) * 0.70)
d.train <- d.bike.easy[train.index, ]
d.test <- d.bike.easy[-train.index, ]

rf.fit <- randomForest(cnt ~ ., data=d.train, ntree=50)
rf.pred <- predict(rf.fit, d.test)

# Compute RMSE
rf.rmse <- sqrt(mean((rf.pred - d.test$cnt)^2))
cat(paste0("RMSE = ", rf.rmse, "\n"))

# Plot first 100 points in test dataset and compare with predictions
show.ind <- 1:100
plot(show.ind, d.test$cnt[show.ind], type="o")
points(show.ind, rf.pred[show.ind], type="o", col="red")

max.cnt <- max(d.test$cnt[show.ind])

lines(show.ind, d.test$atemp[show.ind]*max.cnt, col="blue")
lines(show.ind, d.test$hum[show.ind]*max.cnt, col="darkgreen")
lines(show.ind, d.test$windspeed[show.ind]*max.cnt, col="purple")
points(show.ind, d.test$workingday[show.ind]*max.cnt, col="orange")


### Let's train for regression on "cnt" with a neural network now!

d.bike.short <- d.bike[1:4000, -c(1,2,3,8, 10, 15,16)]
train.index <- 1:(nrow(d.bike.short) * 0.70)
d.train <- d.bike.short[train.index, ]
d.test <- d.bike.short[-train.index, ]

# Neural net parameters
h.size <- 15
maxit <- 500
decay <- 0.001

# Train neural net
nn.fit <- nnet(cnt ~ ., data=d.train,
               #Wts=rep(1/2, (ncol(d.bike.short)+1)*h.size+(h.size+1)*1),
               size=h.size,
               decay=decay,
               linout=TRUE)
nn.pred <- predict(nn.fit, d.test)

# Compute RMSE
nn.rmse <- sqrt(mean((nn.pred - d.test$cnt)^2))
cat(paste0("RMSE = ", nn.rmse, "\n"))

# Plot first 100 points in test dataset and compare with predictions
show.ind <- 1:(8*24)
plot(show.ind, d.test$cnt[show.ind], type="o")
points(show.ind, nn.pred[show.ind], type="o", col="red")
max.cnt <- max(d.test$cnt[show.ind])
lines(show.ind, d.bike[row.names(d.test[show.ind,]),"weekday"]/6*max.cnt, col="blue")

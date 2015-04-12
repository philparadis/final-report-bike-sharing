source("bike-load.R")

library(randomForest)
library(nnet)

# Define some helper functions to calculate errors
# Compute RMSE
compute.rmse <- function(pred, actual)
{
  sqrt(mean((pred-actual)^2))
}
# Compute RMSLE
compute.rmsle <- function(pred, actual)
{
  sqrt(mean((log(pred+1)-log(actual+1))^2))
}

### Example of training a classifier for regression on "cnt"

# For simplicity of using the classifier, remove column "instant", 
# "dteday" as well as all categorical variables with more than 2
# categories. Also remove "casual" and "registered" since their sum
# gives "cnt".
d.bike.easy <- bike.hourly[,c("hr","weekday","weathersit","atemp","cnt")]

train.index <- 1:(nrow(d.bike.easy) * 0.70)
train <- d.bike.easy[train.index, ]
test <- d.bike.easy[-train.index, ]

rf.fit <- randomForest(cnt ~ ., data=train, ntree=50)
rf.pred <- predict(rf.fit, test)

# Plot first 100 points in test dataset and compare with predictions
show.ind <- 1:100
plot(show.ind, test$cnt[show.ind], type="o")
points(show.ind, rf.pred[show.ind], type="o", col="red")

# Plot other properties of the data. For example, temperature.
max.cnt <- max(test$cnt[show.ind])
lines(show.ind, test$atemp[show.ind]*max.cnt, col="blue")

# Compute RMSE and RMSLE
rf.rmse <- compute.rmse(rf.pred, test$cnt)
rf.rmsle <- compute.rmsle(rf.pred, test$cnt)
cat(paste0("RMSE = ", rf.rmse, "\n"))
cat(paste0("RMSLE = ", rf.rmsle, "\n"))

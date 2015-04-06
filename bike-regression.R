source("bike-load.R")

### Example of training a classifier for regression on "cnt"

# For simplicity of using the classifier, remove column "instant", 
# "dteday" as well as all categorical variables with more than 2
# categories. Also remove "casual" and "registered" since their sum
# gives "cnt".
d.bike.easy <- d.bike[,-c(1,2,3,8, 10, 15,16)]

train.index <- 1:(nrow(d.bike.easy) * 0.70)
d.train <- d.bike.easy[train.index, ]
d.test <- d.bike.easy[-train.index, ]

rf.fit <- randomForest(cnt ~ ., data=d.train, ntree=25)
rf.pred <- predict(rf.fit, d.test)

# Compute RMSE
rf.rmse <- sqrt(mean((rf.pred - d.test$cnt)^2))
cat(paste0("RMSE = ", rf.rmse, "\n"))

# Plot first 100 points in test dataset and compare with predictions
show.ind <- 1:100
plot(show.ind, d.test$cnt[show.ind], type="o")
points(show.ind, rf.pred[show.ind], type="o", col="red")

# Plot other properties of the data. For example, temperature.
max.cnt <- max(d.test$cnt[show.ind])
lines(show.ind, d.test$atemp[show.ind]*max.cnt, col="blue")

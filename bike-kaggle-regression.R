if (!exists("bike.hourly"))
  source("bike-load.R")

library(randomForest)
library(nnet)
library(neuralnet)

# Split the training and testing data as in the Kaggle competition, as
# explained here: https://www.kaggle.com/c/bike-sharing-demand/data
# In other words: Days 1 to 19 of each month form the training set.
# Days 20 until last day of the month for each month form the testing set.

is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  
kaggle.training <- bike.hourly[is.training.day,]
kaggle.testing <- bike.hourly[!is.training.day,]

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

run.rf <- function ()
{
  ### TRAIN RANDOM FORESTS
  # Select features
  features <- c("season","yr","mnth","hr","holiday","weekday","weathersit","atemp","cnt")
  train <- kaggle.training[,features]
  test <- kaggle.testing[,features]
  
  rf.fit <- randomForest(cnt ~ ., data=train, ntree=150)
  rf.pred <- predict(rf.fit, test)
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  # Plot 5 days
  begin <- 11*24
  end <- 17*24
  show.ind <- begin:end
  oldpar <- par(mfrow=c(1,1))
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, rf.pred[show.ind], type="o", col="red")
  
  # Plot other properties of the data. For example, temperature.
  #max.cnt <- max(test$cnt[show.ind])
  #lines(show.ind, test$atemp[show.ind]*max.cnt, col="blue")
  par(oldpar)
  
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(rf.pred, test$cnt)
  rf.rmsle <- compute.rmsle(rf.pred, test$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
}


run.nnet <- function ()
{
  ### TRAIN NEURAL NETWORKS
  # Select features
  features <- c("yr","mnth","hr","holiday","weekday","weathersit","atemp","cnt")
  train <- kaggle.training[,features]
  test <- kaggle.testing[,features]
  
  # Turn "hr" into factor
#   train$hr <- factor(train$hr, levels=0:23,
#                      labels=paste0("hr", 0:23))
#   test$hr <- factor(test$hr, levels=0:23,
#                      labels=paste0("hr", 0:23))
  
  # Rescale "hr"
#   scale.hr <- sd(bike.hourly$hr)*2
#   train$hr <- train$hr / scale.hr
#   test$hr <- test$hr / scale.hr
  scale.hr <- 24
  train$hr <- train$hr / scale.hr
  test$hr <- test$hr / scale.hr

  # Scale 'cnt' between 0-1
  scale <- max(train$cnt)
  train$cnt <- train$cnt/scale
  
  fit <- nnet(cnt ~ ., data=train,
              decay=0.0001,
              size=40,
              maxit=1000,
              MaxNWTs=5000)
  pred <- predict(fit, test)
  
  # Scale back 'cnt'
  pred <- pred*scale
  
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1))
  # Plot 5 days
  begin <- 11*24
  end <- 17*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind], type="o", col="red")
  begin <- 250*24
  end <- 256*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind], type="o", col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(pred, test$cnt)
  rf.rmsle <- compute.rmsle(pred, test$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
}


run.neuralnet <- function ()
{
  ### TRAIN NEURAL NETWORKS
  # Select features
  features.remove <- c("instant", "date", "datetime", "temp", "windspeed", "casual", "registered")
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  train <- bike.hourly.binarized[is.training.day,
                                 -which(names(bike.hourly.binarized) %in% features.remove)]
  test <- bike.hourly.binarized[!is.training.day,
                                -which(names(bike.hourly.binarized) %in% features.remove)]
 
  # Scale 'cnt' between 0-1
  scale <- max(train$cnt)
  train$cnt <- train$cnt/scale
  
  n <- names(train)
  f <- as.formula(paste('cnt ~', paste(n[!n %in% 'cnt'], collapse = ' + ')))
  set.seed(0)
  fit <- neuralnet(f, data=train,
                   hidden=c(5,10),
                   stepmax=100000,
                   lifesign="full", lifesign.step=500)
  pred <- compute(fit, test)
  
  # Scale back 'cnt'
  pred <- pred*scale
  
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1))
  # Plot 5 days
  begin <- 11*24
  end <- 17*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind], type="o", col="red")
  begin <- 250*24
  end <- 256*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind], type="o", col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(pred, test$cnt)
  rf.rmsle <- compute.rmsle(pred, test$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
}
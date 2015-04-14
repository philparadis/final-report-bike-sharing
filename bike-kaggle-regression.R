if (!exists("bike.hourly"))
  source("bike-load.R")
source("bike-helper.R")

library(randomForest)
library(nnet)
library(neuralnet)
library(h2o)

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
  sqrt(mean((pred-actual)^2, na.rm=TRUE))
}
# Compute RMSLE
compute.rmsle <- function(pred, actual)
{
  sqrt(mean((log(pred+1)-log(actual+1))^2, na.rm=TRUE))
}

run.rf <- function ()
{
  ### TRAIN RANDOM FORESTS
  
  bike.transformed <- bike.hourly
  # Transform some featureshe testing set.
  bike.transformed$atemp <- abs(bike.transformed$atemp - 0.7)
  # Transform some featureshe testing set.\
  bike.transformed$hum <- abs(bike.transformed$hum - 0.2)
  # Trabsfirn datetine to a numeric value between 0.0 and 1.0
  first.day <- bike.transformed$datetime[1]
  last.day <- tail(bike.transformed$datetime, 1)
  bike.transformed$datetime <- as.numeric(difftime(bike.transformed$datetime, first.day, units="days")) / 
    as.numeric(difftime(last.day, first.day, units="days"))
  
  # Select features
  features <- c("datetime","yr","mnth","hr","holiday","weekday","weathersit","temp","atemp","hum","cnt")
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  train <- bike.transformed[is.training.day, features]
  test <- bike.transformed[!is.training.day, features]
  
  rf.fit <- randomForest(cnt ~ ., data=train, ntree=250)
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


run.rf.aug <- function ()
{
  ### TRAIN RANDOM FORESTS
  # Select features
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  features <- c("season","yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","cnt",
                paste0("cnt.",1:total.prev.hours,"hr.ago"))
  train <- bike.hourly.augmented[is.training.day, features]
  test <- bike.hourly.augmented[!is.training.day, features]
  
  rf.fit <- randomForest(cnt ~ ., data=train, ntree=350)
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
  
  write.csv(pred, 
}

run.nnet <- function ()
{
  ### TRAIN NEURAL NETWORKS
  
  bike.transformed <- bike.hourly
  # Transform some featureshe testing set.
  bike.transformed$atemp <- abs(bike.transformed$atemp - 0.7)
  # Transform some featureshe testing set.\
  bike.transformed$hum <- abs(bike.transformed$hum - 0.2)
  # Trabsfirn datetine to a numeric value between 0.0 and 1.0
  first.day <- bike.transformed$datetime[1]
  last.day <- tail(bike.transformed$datetime, 1)
  bike.transformed$datetime <- as.numeric(difftime(bike.transformed$datetime, first.day, units="days")) / 
    as.numeric(difftime(last.day, first.day, units="days"))

  # Select features
  features <- c("yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","cnt")
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  train <- bike.transformed[is.training.day, features]
  test <- bike.transformed[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$cnt)
  train$cnt <- train$cnt/scale
  
  fit <- nnet(cnt ~ ., data=train,
              decay=0.001,
              MaxNWts=50000,
              size=40,
              maxit=1000)
  pred <- predict(fit, test)
  
  # Scale back 'cnt'
  pred <- pred*scale
  
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
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

run.nnet.cas.reg <- function ()
{
  ### TRAIN NEURAL NETWORKS
  
  bike.transformed <- bike.hi
  # Transform some featureshe testing set.
  bike.transformed$atemp <- abs(bike.transformed$atemp - 0.7)
  # Transform some featureshe testing set.\
  bike.transformed$hum <- abs(bike.transformed$hum - 0.2)
  # Trabsfirn datetine to a numeric value between 0.0 and 1.0
  first.day <- bike.transformed$datetime[1]
  last.day <- tail(bike.transformed$datetime, 1)
  bike.transformed$datetime <- as.numeric(difftime(bike.transformed$datetime, first.day, units="days")) / 
    as.numeric(difftime(last.day, first.day, units="days"))
  
  # CASUAL
  # Select features
  features <- c("yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","casual")
  is.training.day <- as.POSIXlt(bike.hi$date)$mday <= 19
  train <- bike.transformed[is.training.day, features]
  test <- bike.transformed[!is.training.day, features]
  actual.cnt <- bike.transformed[!is.training.day, "cnt"]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- nnet(casual ~ ., data=train,
              decay=0.001,
              MaxNWts=50000,
              size=40,
              maxit=1000)
  pred <- predict(fit, test)
  
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  features <- c("yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","registered")
  is.training.day <- as.POSIXlt(bike.hi$date)$mday <= 19
  train <- bike.transformed[is.training.day, features]
  test <- bike.transformed[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- nnet(registered ~ ., data=train,
              decay=0.001,
              MaxNWts=50000,
              size=40,
              maxit=1000)
  pred <- predict(fit, test)
  
  # Scale back 'registered'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.registered <- pred
  
  pred <- pred.casual + pred.registered
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
  # Plot 5 days
  begin <- 11*24+1
  end <- 17*24
  plot.cnt(begin, end, actual)
  plot.cnt(begin, end, data.frame(cnt=pred), add=TRUE, col="red")
  begin <- 250*24+1
  end <- 256*24
  plot.cnt(begin, end, actual)
  plot.cnt(begin, end, data.frame(cnt=pred), add=TRUE, col="red")
  
  plot.cnt.dates("2011-11-22", "2011-11-27",
                  cbind(bike.hi[!is.training.day, c("date","weekday")],
                        data.frame(cnt=actual.cnt)))
  plot.cnt.dates("2011-11-22", "2011-11-27",
                 cbind(bike.hi[!is.training.day, c("date","weekday")],
                       data.frame(cnt=pred)),
                 add=TRUE, col="red")
  
  show.ind <- begin:end
  plot(show.ind, actual.cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind], type="o", col="red")
  begin <- 250*24
  end <- 256*24
  show.ind <- begin:end
  plot(show.ind, actual.cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind], type="o", col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(pred, actual$cnt)
  rf.rmse <- compute.rmse(pred, actual$cnt)
  rf.rmsle <- compute.rmsle(pred, actual$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
  
  # Best score: 0.472
}

run.nnet.aug <- function ()
{
  ### TRAIN NEURAL NETWORKS
  # Select features
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  features <- c("season","yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","cnt",
                paste0("cnt.",1:total.prev.hours,"hr.ago"))
  train <- bike.hourly.augmented[is.training.day, features]
  test <- bike.hourly.augmented[!is.training.day, features]
  
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
              decay=0.001,
              size=15,
              maxit=3000,
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


run.RSNNS <- function ()
{
  ### TRAIN NEURAL NETWORKS
  # Select features
  features <- c("yr","mnth","hr","holiday","weekday","weathersit","atemp","cnt","hum")
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
  
  fit <- jordan(train[,-ncol(train)],
                train$cnt,
                size=c(8),
                learnFuncParams=c(0.1),
                maxit=100,
                inputsTest=test[,-ncol(test)],
                targetsTest=test$cnt,
                linOut=FALSE)
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


run.h2o <- function ()
{
  ### TRAIN NEURAL NETWORKS
  
  bike.transformed <- bike.hourly
  # Transform some featureshe testing set.
  bike.transformed$atemp <- abs(bike.transformed$atemp - 0.7)
  # Transform some featureshe testing set.\
  bike.transformed$hum <- abs(bike.transformed$hum - 0.2)
  # Trabsfirn datetine to a numeric value between 0.0 and 1.0
  first.day <- bike.transformed$datetime[1]
  last.day <- tail(bike.transformed$datetime, 1)
  bike.transformed$datetime <- as.numeric(difftime(bike.transformed$datetime, first.day, units="days")) / 
    as.numeric(difftime(last.day, first.day, units="days"))
  
  # Select features
  features <- c("datetime","yr","mnth","hr","holiday","weekday","weathersit","temp","atemp","hum","cnt")
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  train <- bike.transformed[is.training.day, features]
  test <- bike.transformed[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$cnt)
  train$cnt <- train$cnt/scale
  test$cnt <- test$cnt/scale
  
  x <- 1:(ncol(train)-1)
  y <- "cnt"

  train.h2o <- as.h2o(localH2O, train, key='data')
  test.h2o <- as.h2o(localH2O, test, key='test')
#   
#   model <- h2o.deeplearning( x = x,  # column numbers for predictors
#                              y = y,   # column number for label
#                              data = train.h2o, # data in H2O format
#                              classification = FALSE,
#                              activation = "TanhWithDropout",
#                              #activation = "RectifierWithDropout",
#                              input_dropout_ratio = 0.2, # % of inputs dropout
#                              hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
#                              #balance_classes = TRUE, 
#                              #hidden = c(50,50,50), # three layers of 50 nodes
#                              hidden = c(5,5,5),
#                              epochs = 300) # max. no. of epochs
#   
#   #Create a set of network topologies
#   input_dropout_ratio = c(0.2, 0.1, 0.05, 0.01)
#   hidden_dropout_ratios = list(c(0.1,0.1), c(0.05,0.05), c(0.01,0.01))
#   hidden_layers = list(c(10,10), c(50,50), c(100,200), c(300,100))
#   l1_norm = c(0, 1e-5, 1e-7)
#   l2_norm = c(0, 1e-5, 1e-7)
#   model.grid <- h2o.deeplearning(x = x,  # column numbers for predictors
#                                  y = y,   # column number for label
#                                  data = train.h2o, # data in H2O format
#                                  validation = test.h2o,
#                                  classification = FALSE,
#                                  activation = "RectifierWithDropout",
#                                  input_dropout_ratio = 0.1, # % of inputs dropout
#                                  hidden_dropout_ratios = c(0.1,0.1), # % for nodes dropout
#                                  hidden = hidden_layers,
#                                  l1=l1_norm,
#                                  l2=l2_norm,
#                                  epochs = 50) # max. no. of epochs
# 
#   model <- model.grid@model[[1]]


#   model <- h2o.deeplearning(x = x,  # column numbers for predictors
#                             y = y,   # column number for label
#                             data = train.h2o, # data in H2O format
#                             classification = FALSE,
#                             #activation = "TanhWithDropout",
#                             activation = "RectifierWithDropout",
#                             input_dropout_ratio = 0.1, # % of inputs dropout
#                             hidden_dropout_ratios = c(0.3,0.3), # % for nodes dropout
#                             hidden = c(100,100),
#                             l1=1e-1,
#                             l2=1e-3,
#                             epochs = 500, # max. no. of epochs
#                             quiet_mode = FALSE,
#                             rate = 0.00001,
#                             score_interval=2,
#                             force_load_balance=TRUE,
#                             holdout_fraction=0.1,
#                             score_training_samples=0,
#                             score_validation_samples=0)

model <- h2o.deeplearning(x = x,  # column numbers for predictors
                          y = y,   # column number for label
                          data = train.h2o, # data in H2O format
                          validation = test.h2o,
                          classification = FALSE,
                          #activation = "TanhWithDropout",
                          activation = "RectifierWithDropout",
                          input_dropout_ratio = 0.2, # % of inputs dropout
                          hidden_dropout_ratios = c(0.1,0.1), # % for nodes dropout
                          hidden = c(80,80),
                          l1=0,
                          l2=1e-6,
                          epochs = 450, # max. no. of epochs
                          quiet_mode = FALSE,
                          rate = 0.0001,
                          force_load_balance=TRUE,
                          score_training_samples=0,
                          score_validation_samples=0)

  ## Using the DNN model for predictions
  pred.h2o <- h2o.predict(model, test.h2o)
  
  ## Converting H2O format into data frame
  pred <- as.data.frame(pred.h2o)
  
  # Scale back 'cnt'
  test$cnt <- test$cnt*scale
  pred <- pred*scale
  
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
  # Plot 5 days
  begin <- 11*24
  end <- 17*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind,], type="o", col="red")
  begin <- 250*24
  end <- 256*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind,], type="o", col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(pred, test$cnt)
  rf.rmsle <- compute.rmsle(pred, test$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))

#   for (model in model.grid@model) {  ## Using the DNN model for predictions
#     pred.h2o <- h2o.predict(model, test.h2o)
#     pred <- as.data.frame(pred.h2o)
#     pred <- pred*scale
#     rf.rmsle <- compute.rmsle(pred, test$cnt)
#     cat(paste0("RMSLE = ", rf.rmsle, "\n"))
#     cat(paste0("Valid sq error = ", model@model$valid_sqr_error, "\n"))
#   }
}


run.h2o.aug <- function ()
{
  ### TRAIN NEURAL NETWORKS
  # Select features
  is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
  features <- c("season","yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","cnt",
                paste0("cnt.",1:18,"hr.ago"))
  train <- bike.hourly.augmented[is.training.day, features]
  test <- bike.hourly.augmented[!is.training.day, features]
  
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
  
  train.h2o <- as.h2o(localH2O, train, key='data')
  x <- 1:(ncol(train)-1)
  y <- "cnt"
  
  #Create a set of network topologies
  input_dropout_ratio = c(0.2, 0.1, 0.05, 0.01)
  hidden_dropout_ratios = list(c(0.1,0.1), c(0.05,0.05), c(0.01,0.01))
  hidden_layers = list(c(10,10), c(50,50), c(100,200), c(300,100))
  l1_norm = c(0, 1e-5, 1e-7)
  l2_norm = c(0, 1e-5, 1e-7)
  model <- h2o.deeplearning( x = x,  # column numbers for predictors
                             y = y,   # column number for label
                             data = train.h2o, # data in H2O format
                             classification = FALSE,
                             #activation = "TanhWithDropout",
                             activation = "RectifierWithDropout",
                             input_dropout_ratio = 0.02, # % of inputs dropout
                             hidden_dropout_ratios = c(0.06,0.06), # % for nodes dropout
                             #balance_classes = TRUE, 
                             #hidden = c(50,50,50), # three layers of 50 nodes
                             hidden = c(100,200),
                             l1=1e-6,
                             l2=1e-9,
                             epochs = 200) # max. no. of epochs
  
  ## Using the DNN model for predictions
  test.h2o <- as.h2o(localH2O, test, key='data')
  pred.h2o <- h2o.predict(model, test.h2o)
  
  ## Converting H2O format into data frame
  pred <- as.data.frame(pred.h2o)
  
  # Scale back 'cnt'
  pred <- pred*scale
  
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
  # Plot 5 days
  begin <- 11*24
  end <- 17*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind,], type="o", col="red")
  begin <- 250*24
  end <- 256*24
  show.ind <- begin:end
  plot(show.ind, test$cnt[show.ind], type="o",
       main=paste0("Plotting ", kaggle.testing[begin,"date"], " (",
                   kaggle.testing[begin,"weekday"], ") to ", kaggle.testing[end,"date"], " (",
                   kaggle.testing[end, "weekday"], ")"))
  points(show.ind, pred[show.ind,], type="o", col="red")
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
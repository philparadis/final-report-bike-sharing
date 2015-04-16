source("bike-helper.R")
source("bike-features-extract.R")

library(randomForest)
library(nnet)

bike.results <- bike.hfx

# Split the training and testing data as in the Kaggle competition, as
# explained here: https://www.kaggle.com/c/bike-sharing-demand/data
# In other words: Days 1 to 19 of each month form the training set.
# Days 20 until last day of the month for each month form the testing set.

is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19

run.rf.hfx <- function ()
{
  ### TRAIN NEURAL NETWORKS
  
  bike.transformed <- bike.hfx
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
  features <- c("yr","mnth","hr","typeofday","weekday","weathersit","atemp","hum","casual")
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- bike.transformed[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.transformed[!is.training.day, features]
  actual.cnt <- bike.transformed[!is.training.day, "cnt"]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- randomForest(casual ~ ., data=train, ntrees=250)
  pred <- predict(fit, test)
  
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  features <- c("yr","mnth","hr","typeofday","weekday","weathersit","atemp","hum","registered")
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- bike.transformed[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.transformed[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- randomForest(registered ~ ., data=train, ntrees=250)
  pred <- predict(fit, test)
  
  # Scale back 'registered'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.registered <- pred
  
  pred <- pred.casual + pred.registered
  bike.results[!is.training.day,"pred"] <- pred
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
  # Plot 5 days
  date <- as.POSIXct("2011-01-11", tz="UTC")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="cnt")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="pred",
                  add=TRUE, col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(pred, actual$cnt)
  rf.rmse <- compute.rmse(pred, actual$cnt)
  rf.rmsle <- compute.rmsle(pred, actual$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
}

run.nnet.hfx <- function (h.size=25, maxit=500)
{
  ### PARAMETERS
  training.features <-  c("season","yr","mnth","hr","typeofday","weekday","weathersit","atemp","hum","windspeed")
  
  ### TRAIN NEURAL NETWORKS
  
  bike.transformed <- bike.hfx
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
  features <- c(training.features, "casual")
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- bike.transformed[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.transformed[!is.training.day, features]
  actual.cnt <- bike.transformed[!is.training.day, "cnt"]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- nnet(casual ~ ., data=train,
              decay=0.001,
              MaxNWts=50000,
              size=h.size,
              maxit=maxit)
  pred <- predict(fit, test)
  
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  features <- c(training.features, "registered")
  train <- bike.transformed[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.transformed[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- nnet(registered ~ ., data=train,
              decay=0.001,
              MaxNWts=50000,
              size=h.size,
              maxit=maxit)
  pred <- predict(fit, test)
  
  # Scale back 'registered'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.registered <- pred
  
  pred <- pred.casual + pred.registered
  bike.results[!is.training.day,"pred"] <- pred
  
  # Multiply by 0.5 the days around Christmas
  christmas.index <- bike.results$date==as.POSIXct("2011-12-24",tz="UTC") |
                      bike.results$date==as.POSIXct("2011-12-25",tz="UTC") |
                      bike.results$date==as.POSIXct("2011-12-26",tz="UTC") |
                      bike.results$date==as.POSIXct("2012-12-24",tz="UTC") |
                      bike.results$date==as.POSIXct("2011-12-25",tz="UTC") |
                      bike.results$date==as.POSIXct("2012-12-26",tz="UTC")
  bike.results[christmas.index, "pred"] <- 0.5 * bike.results[christmas.index, "pred"]
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
  # Plot 5 days
  date <- as.POSIXct("2011-03-20", tz="UTC")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="cnt")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="pred",
                  add=TRUE, col="red")
  # Christmas period
  date <- as.POSIXct("2011-12-22", tz="UTC")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="cnt")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="pred",
                  add=TRUE, col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(bike.results[!is.training.day, "pred"], actual$cnt)
  rf.rmsle <- compute.rmsle(bike.results[!is.training.day, "pred"], actual$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
  
  # Best score: 0.472
  # NEW best score after fixing "holidays" in feature extraction, removing
  # bad rows, adding "typeofday" and multiplying the count for days around
  # christmas by half: 0.451144782190852
  
  bike.results <<- bike.results
  rf.rmsle
}

# RUN EXPERIMENT
# TODO: Move this into its own file...

params.h.size <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 65, 90)
params.maxit <- c(100, 300, 1000, 2500, 5000)

results.log <- c()
for (maxit in params.maxit) {
  for (h.size in params.h.size) {
    seed <- maxit + h.size
    set.seed(seed)
    runtime <- system.time(rmsle <- run.nnet.hfx(h.size, maxit))[[1]]
    res <- data.frame(datetime=Sys.time(), runtime=runtime,
                      seed=seed, h.size=h.size, maxit=maxit, rmsle=rmsle)
    results.log <- rbind(results.log, res)
    write.csv(results.log, "results.log")
    write.csv(bike.results[!is.training.day, "pred"],
              paste0("pred_s", seed))
  }
}


run.nnet.hfx <- function ()
{
  ### TRAIN NEURAL NETWORKS
  
  bike.transformed <- bike.hfx
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
  features <- c("season","yr","mnth","hr","typeofday","weekday","weathersit","atemp","hum","windspeed","casual")
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- bike.transformed[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.transformed[!is.training.day, features]
  actual.cnt <- bike.transformed[!is.training.day, "cnt"]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  ##############
  train.h2o <- as.h2o(localH2O, train, key='data')
  
  x <- 1:(ncol(train)-1)
  y <- "casual"
  
  # Create a grid of parameters
  input_dropout_ratio = 0.1
  hidden_dropout_ratios = c(0.1, 0.1)
  hidden_layers = c(100,200)
  holdout_fraction = 0.2
  l1_norm = 0
  l2_norm = 1e-7
  
  model.grid <- h2o.deeplearning(x = x,  # column numbers for predictors
                                 y = y,   # column number for label
                                 data = train.h2o, # data in H2O format
                                 classification = FALSE,
                                 activation = "RectifierWithDropout",
                                 input_dropout_ratio = input_dropout_ratio, # % of inputs dropout
                                 hidden_dropout_ratios = hidden_dropout_ratios, # % for nodes dropout
                                 hidden = hidden_layers,
                                 holdout_fraction = holdout_fraction,
                                 l1=l1_norm,
                                 l2=l2_norm,
                                 rate = 0.005,
                                 epochs = 100) # max. no. of epochs
  
  # Pick the best model for the rest of the analysis
  #model <- model.grid@model[[1]]
  model <- model.grid
  
  ## Using the DNN model for predictions
  test.h2o <- as.h2o(localH2O, test, key='data')
  pred.h2o <- h2o.predict(model, test.h2o)
  
  ## Converting H2O format into data frame
  pred <- as.data.frame(pred.h2o)
  
  ###############
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  features <- c("season","yr","mnth","hr","typeofday","weekday","weathersit","atemp","hum","windspeed","registered")
  train <- bike.transformed[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.transformed[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
#####################
  train.h2o <- as.h2o(localH2O, train, key='data')
  
  x <- 1:(ncol(train)-1)
  y <- "registered"
  
  # Create a grid of parameters
  input_dropout_ratio = 0.1
  hidden_dropout_ratios = c(0.1, 0.1)
  hidden_layers = c(100,200)
  holdout_fraction = 0.2
  l1_norm = 0
  l2_norm = 1e-7
  
  model.grid <- h2o.deeplearning(x = x,  # column numbers for predictors
                                 y = y,   # column number for label
                                 data = train.h2o, # data in H2O format
                                 classification = FALSE,
                                 activation = "RectifierWithDropout",
                                 input_dropout_ratio = input_dropout_ratio, # % of inputs dropout
                                 hidden_dropout_ratios = hidden_dropout_ratios, # % for nodes dropout
                                 hidden = hidden_layers,
                                 holdout_fraction = holdout_fraction,
                                 l1=l1_norm,
                                 l2=l2_norm,
                                 rate = 0.005,
                                 epochs = 100) # max. no. of epochs
  
  # Pick the best model for the rest of the analysis
  # model <- model.grid@model[[1]]
  model <- model.grid
  
  ## Using the DNN model for predictions
  test.h2o <- as.h2o(localH2O, test, key='data')
  pred.h2o <- h2o.predict(model, test.h2o)
  
  ## Converting H2O format into data frame
  pred <- as.data.frame(pred.h2o)

#####################

  # Scale back 'registered'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.registered <- pred
  
  pred <- pred.casual + pred.registered
  bike.results[!is.training.day,"pred"] <- pred
  
  # Multiply by 0.5 the days around Christmas
  christmas.index <- bike.results$date==as.POSIXct("2011-12-24",tz="UTC") |
    bike.results$date==as.POSIXct("2011-12-25",tz="UTC") |
    bike.results$date==as.POSIXct("2011-12-26",tz="UTC") |
    bike.results$date==as.POSIXct("2012-12-24",tz="UTC") |
    bike.results$date==as.POSIXct("2011-12-25",tz="UTC") |
    bike.results$date==as.POSIXct("2012-12-26",tz="UTC")
  bike.results[christmas.index, "pred"] <- 0.5 * bike.results[christmas.index, "pred"]
  
  ### PLOT PREDICTIONS AGAINST ACTUAL DATA
  oldpar <- par(mfrow=c(2,1), mar=c(2.1,4.1,2,2.1))
  # Plot 5 days
  date <- as.POSIXct("2011-03-20", tz="UTC")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="cnt")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="pred",
                  add=TRUE, col="red")
  # Christmas period
  date <- as.POSIXct("2011-12-22", tz="UTC")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="cnt")
  bike.plot.dates(date, date + as.difftime(6, unit="days"), bike.results, feature="pred",
                  add=TRUE, col="red")
  par(oldpar)
  
  ### COMPUTE ERROR
  # Compute RMSE and RMSLE
  rf.rmse <- compute.rmse(bike.results[!is.training.day, "pred"], actual$cnt)
  rf.rmse <- compute.rmse(bike.results[!is.training.day, "pred"], actual$cnt)
  rf.rmsle <- compute.rmsle(bike.results[!is.training.day, "pred"], actual$cnt)
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
  
  # Best score: 0.472
  
  bike.results <<- bike.results
}
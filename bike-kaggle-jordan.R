# Jordan Recurrent Neural Network
source("bike-features-extract.R")

library(RSNNS)

# Store results in this data.frame, which is a copy of "bike.hfx",
# but a column "pred" will be cbind'ed to it, containing predictions.
bike.results <- bike.hfx

# Find indices of all rows in the original dataset (i.e. exclude all new
# rows from imputation)
is.not.imputated.row <- bike.hfx$datetime %in% bike.hourly$datetime

run.jordan <- function (hidden.layers = c(15), maxit = 300)
{
  ### PARAMETERS
  
  ### TRAIN RECURRENT NEURAL NETWORKS
  
  # CASUAL
  # Select features
  m <- data.frame(model.matrix( 
    ~ season + yr + mnth + hr + weekday + workingday +
      weathersit + atempdiff + humdiff + windspeed + casual,
    data = bike.hfx
  ))
  n <- colnames(m)[2:(ncol(m)-1)]
  f <- as.formula(paste('casual ~', paste(n[!n %in% 'cnt'], collapse = ' + ')))
  
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- m[is.training.day & bike.hfx$badrow == 0, -1]
  test <- m[!is.training.day, -1]
  
  # Scale 'casual' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- jordan(train[,-ncol(train)],
                train$casual,
                size=hidden.layers,
                learnFuncParams=c(0.2),
                maxit=maxit,
                inputsTest=test[,-ncol(test)],
                targetsTest=test$casual,
                linOut=TRUE)
  
  # Okay, for Jordan neural networks it is important to remember that they maintain a state
  # and remember the last "x" values that the hidden layer neurons have taken. As such,
  # the network should be trained on continuous values...
  # Moreover, the PREDICTION should be taken over continuous values...
  # I.e., we should predict on the whole data set rather than just the testing set (and then
  # simply extract the rows corresponding the the testing set)
  #pred <- predict(fit, test[,-ncol(test)])
  pred <- predict(fit, m[, -c(1, ncol(m))])
  pred <- pred[!is.training.day]
  
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  m <- data.frame(model.matrix( 
    ~ season + yr + mnth + hr + weekday + workingday +
      weathersit + atempdiff + humdiff + windspeed + registered,
    data = bike.hfx
  ))
  n <- colnames(m)[2:(ncol(m)-1)]
  f <- as.formula(paste('registered ~', paste(n[!n %in% 'cnt'], collapse = ' + ')))
  
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- m[is.training.day & bike.hfx$badrow == 0, -1]
  test <- m[!is.training.day, -1]
  
  # Scale 'registered' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- jordan(train[,-ncol(train)],
                train$registered,
                size=hidden.layers,
                learnFuncParams=c(0.2),
                maxit=maxit,
                inputsTest=test[,-ncol(test)],
                targetsTest=test$registered,
                linOut=TRUE)
  #pred <- predict(fit, test[,-ncol(test)])
  pred <- predict(fit, m[, -c(1, ncol(m))])
  pred <- pred[!is.training.day]
  
  # Scale back 'registered'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.registered <- pred
  
  pred <- pred.casual + pred.registered
  bike.results[!is.training.day, "pred"] <- pred
  
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
  rf.rmse <- compute.rmse(bike.results[!is.training.day & is.not.imputated.row, "pred"],
                          bike.results[!is.training.day & is.not.imputated.row, "cnt"])
  rf.rmsle <- compute.rmsle(as.integer(bike.results[!is.training.day & is.not.imputated.row, "pred"]),
                            bike.results[!is.training.day & is.not.imputated.row, "cnt"])
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
  
  # Best score: 0.472
  # NEW best score after fixing "holidays" in feature extraction, removing
  # bad rows and multiplying the count for days around
  # christmas by half: 0.451144782190852
  
  bike.results <<- bike.results
  rf.rmsle
}

run.elman <- function (hidden.layers, maxit)
{
  ### PARAMETERS
  ### TRAIN RECURRENT NEURAL NETWORKS
  
  # CASUAL
  # Select features
  m <- data.frame(model.matrix( 
    ~ season + yr + mnth + hr + weekday + workingday +
      weathersit + atempdiff + humdiff + windspeed + casual,
    data = bike.hfx
  ))
  n <- colnames(m)[2:(ncol(m)-1)]
  f <- as.formula(paste('casual ~', paste(n[!n %in% 'cnt'], collapse = ' + ')))
  
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- m[is.training.day & bike.hfx$badrow == 0, -1]
  test <- m[!is.training.day, -1]
  
  # Scale 'casual' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- elman(train[,-ncol(train)],
                train$casual,
                size=hidden.layers,
                learnFuncParams=c(0.1),
                maxit=maxit,
                inputsTest=test[,-ncol(test)],
                targetsTest=test$casual,
                linOut=FALSE)
  
  # Okay, for Jordan neural networks it is important to remember that they maintain a state
  # and remember the last "x" values that the hidden layer neurons have taken. As such,
  # the network should be trained on continuous values...
  # Moreover, the PREDICTION should be taken over continuous values...
  # I.e., we should predict on the whole data set rather than just the testing set (and then
  # simply extract the rows corresponding the the testing set)
  #pred <- predict(fit, test[,-ncol(test)])
  pred <- predict(fit, m[, -c(1, ncol(m))])
  pred <- pred[!is.training.day]
  
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  m <- data.frame(model.matrix( 
    ~ season + yr + mnth + hr + weekday + workingday +
      weathersit + atempdiff + humdiff + windspeed + registered,
    data = bike.hfx
  ))
  n <- colnames(m)[2:(ncol(m)-1)]
  f <- as.formula(paste('registered ~', paste(n[!n %in% 'cnt'], collapse = ' + ')))
  
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- m[is.training.day & bike.hfx$badrow == 0, -1]
  test <- m[!is.training.day, -1]
  
  # Scale 'registered' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- elman(train[,-ncol(train)],
                train$registered,
                size=hidden.layers,
                learnFuncParams=c(0.1),
                maxit=maxit,
                inputsTest=test[,-ncol(test)],
                targetsTest=test$registered,
                linOut=FALSE)
  #pred <- predict(fit, test[,-ncol(test)])
  pred <- predict(fit, m[, -c(1, ncol(m))])
  pred <- pred[!is.training.day]
  
  # Scale back 'registered'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.registered <- pred
  
  pred <- pred.casual + pred.registered
  bike.results[!is.training.day, "pred"] <- pred
  
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
  rf.rmse <- compute.rmse(bike.results[!is.training.day & is.not.imputated.row, "pred"],
                          bike.results[!is.training.day & is.not.imputated.row, "cnt"])
  rf.rmsle <- compute.rmsle(as.integer(bike.results[!is.training.day & is.not.imputated.row, "pred"]),
                            bike.results[!is.training.day & is.not.imputated.row, "cnt"])
  cat(paste0("RMSE = ", rf.rmse, "\n"))
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
  
  # Best score: 0.472
  # NEW best score after fixing "holidays" in feature extraction, removing
  # bad rows and multiplying the count for days around
  # christmas by half: 0.451144782190852
  
  bike.results <<- bike.results
  rf.rmsle
}

#params.h.size <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 65, 90)
#params.maxit <- c(100, 300, 1000, 2500, 5000)
params.h.layers <- list(12, 18, 24, 30, 36, 42, 48)
params.maxit <- c(500, 3000)

results.log <- c()
for (maxit in params.maxit) {
  for (h.layers in params.h.layers) {
    seed <- maxit + sum(h.layers)
    set.seed(1000000+seed)
    cat(paste0("Running elman RSNNS - h.layers = ", h.size, ", maxit = ", maxit, "\n"))
    runtime <- system.time(rmsle <- run.elman(h.layers, maxit))[[1]]
    res <- data.frame(datetime=Sys.time(), runtime=runtime,
                      seed=seed, h.layers=h.layers, maxit=maxit, rmsle=rmsle)
    results.log <- rbind(results.log, res)
    write.csv(results.log, "results_elman1.log")
    write.csv(bike.results[!is.training.day, "pred"],
              paste0("pred_elman_s", seed))
  }
}



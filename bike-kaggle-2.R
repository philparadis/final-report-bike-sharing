source("bike-helper.R")
source("bike-features-extract.R")

library(randomForest)
library(nnet)
library(neuralnet)

bike.results <- bike.hfx

# Split the training and testing data as in the Kaggle competition, as
# explained here: https://www.kaggle.com/c/bike-sharing-demand/data
# In other words: Days 1 to 19 of each month form the training set.
# Days 20 until last day of the month for each month form the testing set.

is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19

# Find indices of all rows in the original dataset (i.e. exclude all new
# rows from imputation)

is.not.imputated.row <- bike.hfx$datetime %in% bike.hourly$datetime

run.rf.hfx <- function ()
{
  ### TRAIN NEURAL NETWORKS

  # CASUAL
  # Select features
  features <- c("yr","mnth","hr","weekday","workingday","weathersit","atempdiff","humdiff","casual")
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
  features <- c("yr","mnth","hr","weekday","workingday","weathersit","atempdiff","humdiff","registered")
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

run.nnet.hfx <- function (h.size=25, maxit=500, decay=0.001)
{
  ### PARAMETERS
  training.features <-  c("season","yr","mnth","hr","weekday","workingday","weathersit","atempdiff","hum","windspeed")
  
  ### TRAIN NEURAL NETWORKS
  
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
              decay=decay,
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

# RUN EXPERIMENT
# TODO: Move this into its own file...

#params.h.size <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 65, 90)
#params.maxit <- c(100, 300, 1000, 2500, 5000)
params.h.size <- c(3,4,6,7,8,9,11,12,13,14,16,17,18,19)
params.maxit <- c(100, 300, 1000)
params.decay <- c(0.001, 

results.log <- c()
for (maxit in params.maxit) {
  for (h.size in params.h.size) {
    seed <- maxit + h.size
    set.seed(seed)
    runtime <- system.time(rmsle <- run.nnet.hfx(h.size, maxit))[[1]]
    res <- data.frame(datetime=Sys.time(), runtime=runtime,
                      seed=seed, h.size=h.size, maxit=maxit, rmsle=rmsle)
    results.log <- rbind(results.log, res)
    write.csv(results.log, "results2.log")
    write.csv(bike.results[!is.training.day, "pred"],
              paste0("pred_s", seed))
  }
}


run.neuralnet.hfx <- function (h.size=25, maxit=500, decay=0.001)
{
  ### TRAIN NEURAL NETWORKS
  
  # CASUAL
  # Select features
  m <- data.frame(model.matrix( 
    ~ season + yr + mnth + hr + weekday + workingday +
      weathersit + atempdiff + humdiff + windspeed + casual,
    data = bike.hfx
  ))
  n <- colnames(m)[2:(ncol(m)-1)]
  f <- as.formula(paste('casual ~', paste(n[!n %in% 'cnt'], collapse = ' + ')))
  
  is.training.day <- as.POSIXlt(bike.hfx.binarized$date)$mday <= 19
  train <- m[is.training.day & bike.hfx.binarized$badrow == 0, -1]
  test <- m[!is.training.day, -c(1, ncol(m))]

  # Scale 'casual' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- neuralnet(f,
                   data=train,
                   hidden=15,
                   threshold=0.5,
                   stepmax=50000,
                   err.fct="sse",
                   act.fct="logistic",
                   linear.output=TRUE,
                   lifesign="full",
                   lifesign.step=500,
                   startweights=NULL)
  
  pred <- compute(fit, test)$net.result
  
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
  
  is.training.day <- as.POSIXlt(bike.hfx.binarized$date)$mday <= 19
  train <- m[is.training.day & bike.hfx.binarized$badrow == 0, -1]
  test <- m[!is.training.day, -c(1, ncol(m))]
  
  # Scale 'registered' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- neuralnet(f,
                   data=train,
                   hidden=15,
                   threshold=0.5,
                   stepmax=50000,
                   err.fct="sse",
                   act.fct="logistic",
                   linear.output=TRUE,
                   lifesign="full",
                   lifesign.step=500,
                   startweights=NULL)
  
  pred <- compute(fit, test)$net.result
  
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


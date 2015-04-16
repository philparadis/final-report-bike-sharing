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
  train <- bike.hfx[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.hfx[!is.training.day, features]
  actual.cnt <- bike.hfx[!is.training.day, "cnt"]
  
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
  train <- bike.hfx[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.hfx[!is.training.day, features]
  
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
  training.features <-  c("season","yr","mnth","hr","weekday","workingday",
                          "weathersit","atempdiff","hum","windspeed")
  
  ### TRAIN NEURAL NETWORKS
  
  # CASUAL
  # Select features
  features <- c(training.features, "cta1", "cta2", "casual")
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- bike.hfx[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.hfx[!is.training.day, features]
  actual.cnt <- bike.hfx[!is.training.day, "cnt"]
  
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
  train <- bike.hfx[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.hfx[!is.training.day, features]
  
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
params.h.size <- c(4,6,9,13,17,19,22,24)
params.maxit <- c(300, 600)
params.decay <- c(0.1, 0.01, 0.001, 0.0001)

results.log <- c()
for (maxit in params.maxit) {
  for (h.size in params.h.size) {
    for (decay in params.decay) {
      seed <- maxit + h.size
      set.seed(10000+seed)
      runtime <- system.time(rmsle <- run.nnet.hfx(h.size, maxit, decay))[[1]]
      res <- data.frame(datetime=Sys.time(), runtime=runtime,
                        seed=seed, h.size=h.size, maxit=maxit, decay=decay, rmsle=rmsle)
      results.log <- rbind(results.log, res)
      write.csv(results.log, "results3.log")
      write.csv(bike.results[!is.training.day, "pred"],
                paste0("pred_s", seed))
    }
  }
}


library(doMC)
registerDoMC(cores=8)

run.avNNet.hfx <- function (h.size=25, maxit=500, decay=0.001, repeats=8)
{
  ### PARAMETERS
  training.features <-  c("season","yr","mnth","hr","weekday","workingday",
                          "weathersit","atempdiff","hum","windspeed")
  
  ### TRAIN NEURAL NETWORKS
  
  # CASUAL
  # Select features
  features <- c(training.features, "cta1", "cta2", "casual")
  is.training.day <- as.POSIXlt(bike.hfx$date)$mday <= 19
  train <- bike.hfx[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.hfx[!is.training.day, features]
  actual.cnt <- bike.hfx[!is.training.day, "cnt"]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$casual)
  train$casual <- train$casual/scale
  
  fit <- avNNet(casual ~ ., data=train,
                decay=decay,
                MaxNWts=50000,
                size=h.size,
                maxit=maxit,
                repeats=repeats,
                allowParallel=TRUE)
  pred <- predict(fit, test)
  
  # Scale back 'casual'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  pred.casual <- pred
  
  # REGISTERED
  # Select features
  features <- c(training.features, "registered")
  train <- bike.hfx[is.training.day & bike.hfx$badrow == 0, features]
  test <- bike.hfx[!is.training.day, features]
  
  # Scale 'cnt' between 0-1
  scale <- max(train$registered)
  train$registered <- train$registered/scale
  
  fit <- avNNet(registered ~ ., data=train,
                decay=decay,
                MaxNWts=50000,
                size=h.size,
                maxit=maxit,
                repeats=repeats,
                allowParallel=TRUE)
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

params.h.size <- c(4,9,14,19,24,29,34,39)
params.maxit <- c(100, 500, 1000, 2000)
params.decay <- c(0.0001)

repeats <- 8
results.log <- c()
for (maxit in params.maxit) {
  for (h.size in params.h.size) {
    for (decay in params.decay) {
      seed <- maxit + h.size + decay*10000 + repeats*100000
      set.seed(10000000+seed)
      cat(paste0("h.size = ", h.size, ", maxit = ", maxit, ", decay = ", decay, "\n"))
      runtime <- system.time(rmsle <- run.avNNet.hfx(h.size, maxit, decay, repeats))[[1]]
      res <- data.frame(datetime=Sys.time(), runtime=runtime,
                        seed=seed, h.size=h.size, maxit=maxit, decay=decay, repeats=repeats, rmsle=rmsle)
      results.log <- rbind(results.log, res)
      write.csv(results.log, "results_avnnet_1.log")
      write.csv(bike.results[!is.training.day, "pred"],
                paste0("pred_s", seed))
    }
  }
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
                   hidden=10,
                   threshold=0.1,
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
                   hidden=10,
                   threshold=0.1,
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


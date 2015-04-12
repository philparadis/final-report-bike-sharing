######## Start a local cluster with 2GB RAM
library(h2o)
if (!exists("localH2O")) {
  localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                      nthreads=-1,
                      Xmx = '2g')
}
### TRAIN NEURAL NETWORKS

# Transform some featureshe testing set.
bike.transformed <- bike.hourly
bike.transformed$atemp <- bike.transformed$atemp - 0.7

# Select features
features <- c("yr","mnth","hr","holiday","weekday","weathersit","atemp","hum","cnt")
is.training.day <- as.POSIXlt(bike.hourly$date)$mday <= 19
train <- bike.transformed[is.training.day, features]
test <- bike.transformed[!is.training.day, features]

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

# Create a grid of parameters
input_dropout_ratio = c(0.2, 0.1)
hidden_dropout_ratios = list(c(0.5,0.5), c(0.1,0.1))
#hidden_layers = list(c(50,50), c(100,200), c(300,100))
hidden_layers = C(100,200)
holdout_fraction = c(0.1, 0.25)
l1_norm = c(0, 1e-7)
l2_norm = c(0, 1e-10)

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
                               epochs = 1000) # max. no. of epochs

# Save all models to disk
for (model in model.grid@model) {
  h2o.saveModel(model, dir="objects", force=TRUE)
}

# Pick the best model for the rest of the analysis
model <- model.grid@model[[1]]

# Save the grid of models
h2o.saveModel(object = mnist_model_grid, dir = "/objects", force = TRUE)

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

get.test.error <- function(model)
{
  ## Using the DNN model for predictions
  test.h2o <- as.h2o(localH2O, test, key='data')
  pred.h2o <- h2o.predict(model, test.h2o)
  ## Converting H2O format into data frame
  pred <- as.data.frame(pred.h2o)
  # Scale back 'cnt'
  pred <- pred*scale
  # Sanitize pred: any negative value is set to zero
  pred[pred<0] <- 0
  # Compute RMSLE
  rf.rmsle <- compute.rmsle(pred, test$cnt)
  cat(paste0("RMSLE = ", rf.rmsle, "\n"))
}

train.model.more <- function(model, epochs)
{
  train.h2o <- as.h2o(localH2O, train, key='data')
  x <- 1:(ncol(train)-1)
  y <- "cnt"
  
  model <- h2o.deeplearning(x = x,
                            y = y,
                            data = train.h2o, # data in H2O format
                            classification = FALSE,
                            activation = model@model$params$activation,
                            input_dropout_ratio = model@model$params$input_dropout_ratio,
                            hidden_dropout_ratios = model@model$params$hidden_dropout_ratios,
                            hidden = model@model$params$hidden,
                            l1 = model@model$params$l1,
                            l2 = model@model$params$l2,
                            checkpoint = model,
                            epochs = epochs)
  model
}


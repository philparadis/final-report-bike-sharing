# DESCRIPTION:
# This file defines some helper functions related to the Capital Bike Sharing
# dataset, such as plotting "cnt" graphs and compute error measures.

source("bike-imputation.R")


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

# Plot "cnt" for a range of days
bike.plot <- function(index.begin, index.end, data, feature="cnt", add=FALSE, ...)
{
  begin <- index.begin
  end <- index.end
  indices <- begin:end
  if (isTRUE(add))
    points(indices, data[indices, feature], type="o", ...)
  else {
    plot(indices, data[indices, feature], type="o", xaxt="n",
        main=paste0("Plotting ", data[begin,"date"], " (",
                    data[begin,"weekday"], ") to ", data[end,"date"], " (",
                    data[end, "weekday"], ")"),
        ...)
    axis(1, at=seq(begin, end, 24), labels=FALSE)
    axis(1, at=seq(begin+12, end, 24), tick=FALSE,
         labels=paste0(data[seq(begin,end,24), "weekday"],
                       " h=", data[seq(begin,end,24), "holiday"],
                       "\n", data[seq(begin,end,24),"date"]))
  }
}

bike.plot.dates <- function(date.begin, date.end, data, feature="cnt", add=FALSE, ...)
{
  begin <- which(data$date == as.POSIXct(date.begin, tz="UTC"))[[1]]
  end <- which(data$date == as.POSIXct(date.end, tz="UTC"))[[1]]
  end <- end + 23
  bike.plot(begin, end, data=data, feature=feature, add=add, ...)
}
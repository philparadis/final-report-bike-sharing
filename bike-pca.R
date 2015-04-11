source("bike-load.R")

library(FactoMineR)

#######################################################
### Data-preprocessing for PCA and ICA
#######################################################

# In order to run PCA and ICA, we will perfom the following pre-processing
# steps:
# (1) split any categorical variables with more than 2 classes
# into multiple binary features.
# (2) convert date variables into numeric variables
# (3) discard features that are obviously redundant or unwanted

# Note: We use the following trick: with(df, model.matrix(~ myVar + 0))
# to recode a categorical variable "myVar" with N possible labels into
# N binary columns called "myVarlabel1", ..., "myVarlabelN"

bike.hourly.normalized <- bike.hourly.binarized
first.day <- bike.hourly.normalized$datetime[1]
last.day <- tail(bike.hourly.normalized$datetime, 1)
bike.hourly.normalized$datetime <- as.numeric(difftime(bike.hourly.normalized$datetime, first.day, units="days")) / 
                                as.numeric(difftime(last.day, first.day, units="days"))

# PCA
pca <- prcomp(bike.hourly.normalized)

# FAMD
famd <- FAMD(bike.hourly)

#######################################################
# Calculate cumulative proportion of variance explained
#######################################################
# This function computes how many top PCA components are
# necessary to explain a variance of at least 'min.variance'
how.many.pcs.for.variance <- function (pca, n.components, min.variance)
{
  for (pc.index in 1:n.components) {
    cumul <- sum(pca$sdev[1:pc.index]^2) / sum(pca$sdev^2)
    if (cumul >= min.variance) {
      return(pc.index)
    }
  }
  return(-1)
}

# Call the above function
min.var <- 0.8
cat(paste("The first", how.many.pcs.for.variance(pca, ncol(bike.hourly.prepared), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))

# 
# #######################################################
# # Perform PCA
# #######################################################
# # Perform PCA analysis on each training set independently
# # i.e. on each digit independently
# pc.digits <- {}
# prep.out("figures/digits-pca.pdf", height=4)
# par(mfrow=c(2,5), mar=c(4.1,2.1, 2.1, 1.1))
# for (i in 0:9) {
#   # Important: 'center' is set to FALSE. This makes analysis
#   # much simpler, especially since our data is already fairly well
#   # centered.
#   pc.digits[[i+1]] <- prcomp(d.digits[[i+1]], center = FALSE)
#   plot(pc.digits[[i+1]], col = heat.colors(10), main = i)
#   
#   # Uncomment the following to see a summary of the PCA results...
#   # print(summary(pc.digits[[i+1]]))
# }
# dev.off()
# 
# #######################################################
# # Calculate cumulative proportion of variance explained
# #######################################################
# # This function computes how many top PCA components are
# # necessary to explain a variance of at least 'min.variance'
# how.many.pcs.for.variance <- function (min.variance)
# {
#   results <- matrix(NA, 2, 10)
#   for (i in 0:9) {
#     for (pc.index in 1:256) {
#       cumul <- sum(pc.digits[[i+1]]$sdev[1:pc.index]^2) /
#         sum(pc.digits[[i+1]]$sdev^2)
#       if (cumul >= min.variance) {
#         results[,i+1] <- c(i, pc.index)
#         break
#       }
#     }
#   }
#   
#   for (i in 0:9) {
#     print(paste("The first", results[2,i+1],
#                 "principal components of the digit",
#                 i, "explain", cumul, "% of the total variance."))
#   }
#   results
# }
# 
# # Call the above function
# (req.pcs <- how.many.pcs.for.variance(0.95))
# 
# # Produce LaTeX table describing this result
# row.names(req.pcs) <- c("Digit", "# of Components")
# xres <- xtable(req.pcs)
# print(xres, include.colnames = FALSE)
# 
# ####################################################
# # Examine the PCA components one by one and
# # investigate what abstract 'features' they measure
# ####################################################
# num_pcs <- 256
# pc <- array(dim = c(num_pcs, 256, 10),
#             dimnames = list(c(1:num_pcs),1:256,c(0:9)))
# for (j in 1:num_pcs) {
#   for (i in 0:9) {
#     pc[j,,i+1] <- pc.digits[[i+1]]$rotation[,j]
#   }
# }
# 
# # Source: Shirley Mills STAT5703W
# display.mean.pc <- function(pca_comp, digits) {
#   mean <- apply(digits, 2, mean)
#   for (i in 1:15) {
#     image(matrix(mean+(i-8)*pca_comp, 16,16)[,16:1],
#           xaxt = "n", yaxt = "n", col = gray((255:0)/255))
#   }
# }
# 
# # Source: Shirley Mills STAT5703W
# display.pcs <- function (pcnum) {
#   if (dev.cur() == 1) {
#     x11(width = 7, height = 5)
#   }
#   oldpar <- par(mar = c(0, 0, 0, 0))
#   layout(matrix(1:150, 10, 15, byrow = TRUE))
#   for (i in 0:9) {
#     display.mean.pc(pc[pcnum,,i+1], d.digits[[i+1]])
#   }
#   par(oldpar)
# }
# 
# prep.out("figures/top-pcs-digit-0.pdf", height=3)
# plot.images(t(pc.digits[[0+1]]$rotation), 40)
# dev.off()
# 
# prep.out("figures/pc-1.pdf", width=7, height=5)
# display.pcs(1)
# dev.off()
# 
# prep.out("figures/pc-2.pdf", width=7, height=5)
# display.pcs(2)
# dev.off()
# 
# #####################################
# # Reconstruction of digits with PCA
# #####################################
# d.digits.pc <- {}
# for (i in 0:9) {
#   d.digits.pc[[i+1]] <- d.digits[[i+1]]%*%pc.digits[[i+1]]$rotation
# }
# 
# num.cases <- unlist(lapply(d.digits, dim))[seq(1,20,2)]
# num.features <- unlist(lapply(d.digits, dim))[seq(2,21,2)]
# num.table <- xtable(rbind(num.cases, num.features))
# row.names(num.table) <- c("Training Examples", "Features")
# print(num.table)
# 
# # Recreate a digit from some subset of its PCA coefficients
# recreate <- function(pc.range, digit) {
#   tmp <- matrix(0, num.cases[digit+1], 256)
#   tmp <- d.digits.pc[[digit+1]][,pc.range] %*%
#     t(pc.digits[[digit+1]]$rotation[,pc.range])
#   tmp <- tmp/max(abs(range(tmp))) # Scale the data to lie in [-1, 1]
#   tmp
# }
# 
# # Recreate a digit from some subset of its PCA coefficients
# # Also, add center and rescale as necessary
# recreate.clean <- function(pc.range, digit) {
#   tmp <- matrix(0, num.cases[digit+1], 256)
#   tmp <- d.digits.pc[[digit+1]][,pc.range] %*% 
#     t(pc.digits[[digit+1]]$rotation[,pc.range])
#   #Add the center and rescale back data
#   if (!identical(pc.digits[[digit+1]]$scale, FALSE)) {
#     tmp <- scale(tmp, center = FALSE , scale=1/pc.digits[[digit+1]]$scale)
#   }
#   if (!identical(pc.digits[[digit+1]]$center, FALSE)) {
#     # For presentation purposes, we introduce 'clean.coeff', which
#     # takes care of cleaning 
#     clean.coeff <- (256-max(pc.range))/256*-1*pc.digits[[digit+1]]$center
#     tmp <- scale(tmp, center = clean.coeff, scale=FALSE)
#   }
#   tmp <- tmp/max(abs(range(tmp))) # Dcale the data to lie in [-1, 1]
#   tmp
# }
# 
# # Recreate training sets digits using the PCs specified by 'pc.range'
# plot.recreate.all <- function(pc.range) {
#   if (dev.cur() == 1) {
#     x11(width = 8, height = 8/12*10) 
#   }
#   # Create a plot matrix with 144 subplots - plot in row-wise order
#   layout(matrix(1:120, 10, 12, byrow = TRUE))
#   # No margin (see ?par)
#   oldpar <- par(mar = c(0, 0, 0, 0))
#   for (d in 0:9) {
#     recreated.digits <- recreate(pc.range, d)
#     for (i in 1:12) {
#       # xaxt = "n", yaxt = "n" - no axes
#       image(matrix(recreated.digits[i,],16,16)[,16:1],
#             xaxt = "n", yaxt = "n", col = gray((255:0)/255))
#     }
#   }
#   par(oldpar)
# }
# 
# # Recreate training sets digits using the PCs specified by 'pc.range'
# plot.recreate.gradual <- function(digit) {
#   if (dev.cur() == 1) {
#     x11(width = 9, height = 8/12*10) 
#   }
#   # Create a plot matrix with 144 subplots - plot in row-wise order
#   layout(matrix(1:130, 10, 13, byrow = TRUE))
#   # No margin (see ?par)
#   oldpar <- par(mar = c(0, 0, 0, 0))
#   plot.new()
#   text(0.5, 0.5, labels="Origi-\nnal", cex=2)
#   for (i in 1:12) {
#     image(matrix(d.digits[[digit+1]][i,],16,16)[,16:1],
#           xaxt="n", yaxt="n", col = gray((255:0)/255))
#   }
#   for (num.pcs in c(1,2,3,5,15,30,75,150,256)) {
#     pc.range <- 1:num.pcs
#     recreated.digits <- recreate.clean(pc.range, digit)
#     plot.new()
#     text(0.5, 0.5, paste(num.pcs, "\nPCs"), cex=2)
#     for (i in 1:12) {
#       # xaxt = "n", yaxt = "n" - no axes
#       image(matrix(recreated.digits[i,],16,16)[,16:1], 
#             xaxt = "n", yaxt = "n", col = gray((255:0)/255))
#     }
#   }
#   par(oldpar)
# }
# 
# prep.out("figures/recreate-gradual-digit-0.pdf", width=9, height=7)
# plot.recreate.gradual(0)
# dev.off()
source("bike-features-extract.R")

library(FactoMineR)

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


#######################################################
# PCA
#######################################################

# CASUAL
# Select features
m.casual <- data.frame(model.matrix( 
  ~ season + yr + mnth + hr + weekday + workingday +
    weathersit + atempdiff + humdiff + windspeed + casual,
  data = bike.hfx
))
m.registered <- data.frame(model.matrix( 
  ~ season + yr + mnth + hr + weekday + workingday +
    weathersit + atempdiff + humdiff + windspeed + registered,
  data = bike.hfx
))
m.cnt <- data.frame(model.matrix( 
  ~ season + yr + mnth + hr + weekday + workingday +
    weathersit + atempdiff + humdiff + windspeed + cnt,
  data = bike.hfx
))

# Compute PCA
pca.casual <- prcomp(m.casual[2:ncol(m.casual)], center=TRUE, scale.=TRUE)
pca.registered <- prcomp(m.registered[2:ncol(m.registered)], center=TRUE, scale.=TRUE)
pca.cnt <- prcomp(m.cnt[2:ncol(m.cnt)], center=TRUE, scale.=TRUE)

# FAMD
famd <- FAMD(bike.hourly)

# Get number of PCs to keep for 80% variance
min.var <- 0.8
cat(paste("The first", how.many.pcs.for.variance(pca.casual, ncol(m.casual[2:ncol(m.casual)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
cat(paste("The first", how.many.pcs.for.variance(pca.registered, ncol(m.registered[2:ncol(m.registered)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
cat(paste("The first", how.many.pcs.for.variance(pca.cnt, ncol(m.cnt[2:ncol(m.cnt)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))

#######################################################
# PCA (Predictor variables ONLY)
#######################################################

m.predictors <- data.frame(model.matrix( 
  ~ season + yr + mnth + hr + weekday + workingday +
    weathersit + atempdiff + humdiff + windspeed,
  data = bike.hfx
))

# Compute PCA
pca.predictors <- prcomp(m.predictors[2:ncol(m.predictors)], center=TRUE, scale.=TRUE)

# Get number of PCs to keep for 60% variance
min.var <- 0.5
cat(paste("The first", how.many.pcs.for.variance(pca.predictors, ncol(m.predictors[2:ncol(m.predictors)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
# Get number of PCs to keep for 60% variance
min.var <- 0.6
cat(paste("The first", how.many.pcs.for.variance(pca.predictors, ncol(m.predictors[2:ncol(m.predictors)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
# Get number of PCs to keep for 70% variance
min.var <- 0.7
cat(paste("The first", how.many.pcs.for.variance(pca.predictors, ncol(m.predictors[2:ncol(m.predictors)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
# Get number of PCs to keep for 95% variance
min.var <- 0.8
cat(paste("The first", how.many.pcs.for.variance(pca.predictors, ncol(m.predictors[2:ncol(m.predictors)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
# Get number of PCs to keep for 90% variance
min.var <- 0.9
cat(paste("The first", how.many.pcs.for.variance(pca.predictors, ncol(m.predictors[2:ncol(m.predictors)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))
# Get number of PCs to keep for 95% variance
min.var <- 0.95
cat(paste("The first", how.many.pcs.for.variance(pca.predictors, ncol(m.predictors[2:ncol(m.predictors)]), min.var),
          "principal components are required to explain",
          min.var*100.0, "% of the total variance.\n"))



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
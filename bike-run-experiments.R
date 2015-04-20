source("bike-kaggle-2.R")
source("bike-kaggle-pca.R")
source("bike-kaggle-elman.R")

###########################################
# run.rf.hfx
###########################################
results.rf.hfx <- run.rf.hfx()
saveRDS(results.rf.hfx, file="objects/results.rf.hfx")

###########################################
# run.nnet.hfx
###########################################

if (file.exists("objects/results.nnet.hfx")) {
  results.nnet.hfx <- readRDS("objects/results.nnet.hfx")
} else {
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
  
  results.nnet.hfx <- results.log
  saveRDS(results.nnet.hfx, file="objects/results.nnet.hfx")
}

############################################
# run.avNNet.hfx
############################################
if (file.exists("objects/results.avnnet.hfx")) {
  results.avnnet.hfx <- readRDS("objects/results.avnnet.hfx")
} else {
  library(doMC)
  registerDoMC(cores=8)
  
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
                  paste0("pred_avnnet_s", seed))
      }
    }
  }
  
  results.avnnet.hfx <<- results.log
  saveRDS(results.avnnet.hfx, file="objects/results.avnnet.hfx")
}

############################################
# run.pca.avNNet.hfx
############################################
if (file.exists("objects/results.pca.avnnet.hfx")) {
  results.pca.avnnet.hfx <- readRDS("objects/results.pca.avnnet.hfx")
} else {
  params.h.size <- c(4,9,14)
  params.maxit <- c(1000)
  params.decay <- c(0.0001)
  params.num.features <- c(19, 24, 29, 33, 38, 41)
  
  repeats <- 8
  results.log <- c()
  for (maxit in params.maxit) {
    for (h.size in params.h.size) {
      for (decay in params.decay) {
        for (num.PCs in params.num.features) {
          seed <- h.size + 1000*num.PCs
          set.seed(10000000+seed)
          cat(paste0("h.size = ", h.size, ", maxit = ", maxit, ", decay = ", decay, ", num.PCs = ", num.PCs, "\n"))
          runtime <- system.time(rmsle <- run.pca.avNNet.hfx(h.size, maxit, decay, repeats, num.PCs))[[1]]
          res <- data.frame(datetime=Sys.time(), runtime=runtime,
                            seed=seed, h.size=h.size, maxit=maxit, decay=decay, repeats=repeats, num.PCs=num.PCs, rmsle=rmsle)
          results.log <- rbind(results.log, res)
          write.csv(results.log, "results_pca_avnnet_1.log")
          write.csv(bike.results[!is.training.day, "pred"],
                    paste0("pred_pca_avnnet_s", seed))
        }
      }
    }
  }
  
  results.pca.avnnet.hfx <<- results.log
  saveRDS(results.pca.avnnet.hfx, file="objects/results.pca.avnnet.hfx")
}


############################################
# run.elman
############################################
if (file.exists("objects/results.elman")) {
  results.elman <- readRDS("objects/results.elman")
} else {
  params.h.layers <- list(12, 18, 24, 30, 36, 42, 48, 60, 90)
  params.maxit <- c(500, 3000)
  
  results.log <- c()
  for (maxit in params.maxit) {
    for (h.layers in params.h.layers) {
      seed <- maxit + sum(h.layers)
      set.seed(1000000+seed)
      cat(paste0("Running elman RSNNS - h.layers = ", h.layers, ", maxit = ", maxit, "\n"))
      runtime <- system.time(rmsle <- run.elman(h.layers, maxit))[[1]]
      res <- data.frame(datetime=Sys.time(), runtime=runtime,
                        seed=seed, h.layers=h.layers, maxit=maxit, rmsle=rmsle)
      results.log <- rbind(results.log, res)
      write.csv(results.log, "results_elman1.log")
      write.csv(bike.results[!is.training.day, "pred"],
                paste0("pred_elman_s", seed))
    }
  }
  
  results.elman <<- results.log
  saveRDS(results.elman, file="objects/results.elman")
}

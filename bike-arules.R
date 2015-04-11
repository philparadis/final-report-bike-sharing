source("bike-load.R")

library(arules)

bike.hourly.arules <- bike.hourly[,c(4,5,6,7,8,9,10,11)]
bike.hourly.arules$hr <- factor(bike.hourly.arules$hr, levels=0:23, labels=as.character(0:23))
bike.hourly.arules <- cbind(bike.hourly.arules,
                            data.frame(atemp=ordered(cut_interval(bike.hourly$atemp, 3), labels=c("low", "med", "high")),
                                       temp=ordered(cut_interval(bike.hourly$temp, 3), labels=c("low", "med", "high")),
                                       hum=ordered(cut_interval(bike.hourly$hum, 3), labels=c("low", "med", "high")),
                                       windspeed=ordered(cut_interval(bike.hourly$windspeed, 3), labels=c("low", "med", "high"))))

# For each row, copy "cnt" number of time that rows
# i.e. each bike rental is one single transaction... so each hour,
# there are "cnt" identical transactions created
counts <- bike.hourly$cnt
trans.indices <- c()
current <- 1
sapply(counts, function(x)
  {
    trans.indices <<- c(trans.indices, rep(current, x))
    current <<- current + 1
  })

# Generate the data.frame of transactions
bike.hourly.fulltransactions <- bike.hourly.arules[trans.indices, ]
row.names(bike.hourly.fulltransactions) <- NULL

# Generate the transactions in the arules format
bikeTrans <- as(bike.hourly.fulltransactions, "transactions")

# Run apriori algorithm on the ~3.3 million transactions
bikeRules <- apriori(bikeTrans, parameter = list(supp = 0.0005,
                                                 conf = 0.01,
                                                 target="rules"))

# Inspect the results
inspect(head(sort(bikeRules, by="lift"), 500))

# Inspect subset of resulting rules
inspect(head(sort(subset(bikeRules,
                        subset = rhs %pin% "temp=" |
                          rhs %pin% "hum=" |
                          rhs %pin% "womdspeed"),
                  by="lift"),
             100))

inspect(head(subset(bikeRules, subset = rhs %pin% "temp="), 100))



# hyperlift
rules.h <- apriori(bikeTrans, parameter = list(supp = 0.00005,
                                               conf = 0.01,
                                               target="rules"))
# Add hyperlift to the set of measures
hyperLift = interestMeasure(rules.h, method = "hyperLift",
                            d=0.9999, transactions = bikeTrans)
quality(rules.h) <- cbind(quality(rules.h), hyperLift = hyperLift)
inspect(head(sort(rules.h, by = "hyperLift"), 100))


))) #error on purpose

# # C++ version
# ibrary(Rcpp)
# #> 
# #> Attaching package: 'Rcpp'
# #> 
# #> The following object is masked from 'package:inline':
# #> 
# #>     registerPlugin
# cppFunction('int add(int x, int y, int z) {
#   int sum = x + y + z;
#   return sum;
# }')
# # add works like a regular R function
# add
# #> function (x, y, z) 
# #> .Primitive(".Call")(<pointer: 0x7f79597337c0>, x, y, z)
# add(1, 2, 3)
# #> [1] 6

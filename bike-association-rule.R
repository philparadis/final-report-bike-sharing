source("bike-load.R")

head(bike.daily)
names(bike.daily)

bike.daily.arule<-bike.daily
head(bike.daily.arule)

#delete some unrelated attributes
bike.daily.arule[["instant"]] <- NULL
bike.daily.arule[["dteday"]] <- NULL
bike.daily.arule[["yr"]] <- NULL
bike.daily.arule[["atemp"]] <- NULL
bike.daily.arule[["casual"]] <- NULL
bike.daily.arule[["registered"]] <- NULL
bike.daily.arule[["date"]] <- NULL

#change numeric attributes into categories
#convert normalized temp.hum, and windspeed back
bike.daily.arule[["temp"]]<-bike.daily[["temp"]]*41
bike.daily.arule[["hum"]]<-bike.daily[["hum"]]*100
bike.daily.arule[["windspeed"]]<-bike.daily[["windspeed"]]*67

#put temp, hum, windspeed into three categories: low, medium, high
bike.daily.arule[["temp"]] <- ordered(cut_interval(bike.daily.arule$temp, 3), labels=c("low","medium","high"))
bike.daily.arule[["hum"]] <- ordered(cut_interval(bike.daily.arule$hum, 3), labels=c("low","medium","high"))
bike.daily.arule[["windspeed"]] <- ordered(cut_interval(bike.daily.arule$windspeed, 3), labels=c("low","medium","high"))
bike.daily.arule[["cnt"]] <- ordered(cut_interval(bike.daily.arule$cnt, 3), labels=c("low","medium","high"))

#convert data into transaction format
head(bike.daily.arule)
library(arules)
bike.arule<- as(bike.daily.arule, "transactions")

bike.arule
summary(bike.arule)

itemFrequencyPlot(bike.arule[, itemFrequency(bike.arule) > 0.1],
                   cex.names = 0.8)
rules <- apriori(bike.arule, parameter = list(support = 0.01,
                                         confidence = 0.6))
rulesCntHighR <- subset(rules, subset = rhs %in% "cnt=high" & lift > 1.2)
inspect(sort(rulesCntHighR, by = "lift")[1:20])

rules <- apriori(bike.arule, parameter = list(support = 0.01,
                                              confidence = 0.6),appearance = list(default="rhs",lhs="cnt=high"), 
                                              control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="lift")

inspect(rules[1:10])



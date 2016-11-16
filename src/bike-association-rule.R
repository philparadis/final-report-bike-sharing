source("bike-load.R")

head(bike.daily)
names(bike.daily)

bike.daily.arule<-bike.daily
head(bike.daily.arule)

#delete some unrelated attributes
bike.daily.arule[["instant"]] <- NULL
bike.daily.arule[["dteday"]] <- NULL
#bike.daily.arule[["casual"]] <- NULL
#bike.daily.arule[["registered"]] <- NULL
bike.daily.arule[["date"]] <- NULL


#change numeric attributes into categories
#convert normalized temp.hum, and windspeed back
#bike.daily.arule[["temp"]]<-bike.daily[["temp"]]*41
#bike.daily.arule[["hum"]]<-bike.daily[["hum"]]*100
#bike.daily.arule[["windspeed"]]<-bike.daily[["windspeed"]]*67

#put temp, hum, windspeed, cnt into three categories: low, medium, high
bike.daily.arule[["atemp"]] <- ordered(cut_interval(bike.daily.arule$atemp, 3), labels=c("low","medium","high"))
bike.daily.arule[["temp"]] <- ordered(cut_interval(bike.daily.arule$temp, 3), labels=c("low","medium","high"))
bike.daily.arule[["hum"]] <- ordered(cut_interval(bike.daily.arule$hum, 3), labels=c("low","medium","high"))
bike.daily.arule[["windspeed"]] <- ordered(cut_interval(bike.daily.arule$windspeed, 3), labels=c("low","medium","high"))

bike.daily.arule[["cnt"]] <- ordered(cut_interval(bike.daily.arule$cnt, 3), labels=c("low","medium","high"))
bike.daily.arule[["casual"]] <- ordered(cut_interval(bike.daily.arule$casual, 3), labels=c("low","medium","high"))
bike.daily.arule[["registered"]] <- ordered(cut_interval(bike.daily.arule$registered, 3), labels=c("low","medium","high"))
#convert data into transaction format
head(bike.daily.arule)
library(arules)
bike.arule<- as(bike.daily.arule, "transactions")

head(bike.arule)
summary(bike.arule)

#itemFrequencyPlot(bike.arule[, itemFrequency(bike.arule) > 0.1],
                   #cex.names = 0.8)
rules <- apriori(bike.arule, parameter = list(support = 0.01,confidence = 0.3))
rulesCntHighR <- subset(rules, subset = rhs %in% "cnt=high" & lift > 1.2)
inspect(sort(rulesCntHighR, by = "lift")[1:10])

rulesRegHighR <- subset(rules, subset = rhs %in% "registered=high")
inspect(sort(rulesRegHighR, by = "lift")[1:10])

rulesCHighR <- subset(rules, subset = rhs %in% "casual=high")
inspect(sort(rulesCHighR, by = "lift")[1:10])






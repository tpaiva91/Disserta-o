setwd("~/Tese/Tese/Databases/CSV/Data")

joaquim <- read.csv("PauloDaily.csv")

joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$Day))


joaquim$Had_Exercise <- as.factor(joaquim$Had_Exercise)
joaquim$Day <- as.factor(joaquim$Day)
joaquim$Max_Value <- as.factor(joaquim$Max_Value)
joaquim$Min_Value <- as.factor(joaquim$Min_Value)
joaquim$Had_ExerciseDayBefore <- as.factor(joaquim$Had_ExerciseDayBefore)
joaquim$Hyperglycemia <- as.factor(joaquim$Hyperglycemia)
joaquim$Hypoglycemia <- as.factor(joaquim$Hypoglycemia)

joaquim$Max_Value <- NULL
joaquim$Min_Value <- NULL

library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.03))
rules.sub <- subset(rules, subset = rhs %in% "Hyperglycemia=1")
## Environment and libraries


Sys.setenv(LANG = "en")
Sys.setenv(LANGUAGE = "en")

library(doParallel)



## Loading data


WalmartTrain <- read.csv("train.csv", stringsAsFactors = FALSE)
WalmartTest <- read.csv("test.csv", stringsAsFactors = FALSE)


## Exploratory Data Analysis

str(WalmartTrain)
str(WalmartTest)
# "Weekday" and "DepartmentDescription" are chr variables -> change to factor ordered

summary(WalmartTrain)
# 4129 Upc values are NAs and 4129 FinelineNumber are NAs (same observations ?)
length(WalmartTrain$FinelineNumber[is.na(WalmartTrain$Upc)]) == 4129
# NAs in Upc and FinelineNumber correspond

summary(WalmartTest)
# 3986 Upc values are NAs and 3986 FinelineNumber are NAs (same observations ?)
length(WalmartTest$FinelineNumber[is.na(WalmartTest$Upc)]) == 3986
# NAs in Upc and FinelineNumber correspond

WalmartTrain$Weekday <- factor(WalmartTrain$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
WalmartTest$Weekday <- factor(WalmartTest$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
# Convert "Weekday" to ordered factor

WalmartTrain$DepartmentDescription <- factor(WalmartTrain$DepartmentDescription)
WalmartTest$DepartmentDescription <- factor(WalmartTest$DepartmentDescription, levels = levels(WalmartTrain$DepartmentDescription))
# Convert "DepartmentDescription" to factor (add new level to WalmartTest)

pairs(WalmartTrain[1:10000, -c(3, 6)])
# No feature seem particularly correlated

cor(WalmartTrain[, -c(3, 6)])
# Only "ScanCount" seem lightly correlated with response (NA values !)

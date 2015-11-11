## Environment and libraries


Sys.setenv(LANG = "en")
Sys.setenv(LANGUAGE = "en")

library(doParallel)
library(ggplot2)
library(gridExtra)
library(plyr)
library(caret)
library(rpart)


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

plotCols <- c("VisitNumber", "Upc", "ScanCount", "FinelineNumber")
plotEE <- function(x){
  title <- paste("TripType vs", x, "\n conditioned on Weekday and DepartmentDescription")
  ggplot(WalmartTrain, aes_string(x, "TripType")) +
    geom_point() +
    facet_grid(Weekday ~ DepartmentDescription) +
    ggtitle(title) +
    stat_smooth(method = "lm")
}
lapply(plotCols, plotWalmart)

plotCols2 <- c("VisitNumber", "Upc", "ScanCount", "FinelineNumber", "TripType")
histWalmart <- function(x){
  title <- paste("Histogram of", x, "conditioned on Weekday")
  ggplot(WalmartTrain, aes_string(x)) +
    geom_histogram(aes(y = ..density..)) +
    facet_grid(. ~ Weekday) +
    ggtitle(title) +
    geom_density()
}
lapply(plotCols2, histWalmart)
# We may notice some differences between the Weekdays for every variable

boxWalmart <- function(x){
  title <- paste("Boxplot of", x, "by Weekday")
  ggplot(WalmartTrain, aes_string('Weekday', x)) +
    geom_boxplot() +
    ggtitle(title)
}
lapply(plotCols2, boxWalmart)
# Only VisitNumber seem slightly different

ggplot(WalmartTrain, aes(x = DepartmentDescription, y = TripType)) + geom_boxplot()
# Two values of DepartmentDescription seem far higher than the rest

WalmartMedian <- ddply(WalmartTrain, c("DepartmentDescription"), summarise, median = median(TripType))
WalmartMedian[order(-WalmartMedian$median), ]
# Higher median factors are "CONCEPT STORES", "OTHER DEPARTMENTS" and "HEALTH AND BEAUTY AIDS"


## Inputing missing values


WalmartTest$TripType <- NA
WalmartCombi <- rbind(WalmartTrain, WalmartTest)

# Upc NA values

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
set.seed(1455)
Upc.input <- rpart(Upc ~ VisitNumber + Weekday + ScanCount + DepartmentDescription, data = WalmartCombi[!is.na(WalmartCombi$Upc), ], method = "anova", control = rpart.control(minsplit = 10, cp = 0.0001))
stopCluster(cl)
rm(cl)

plotcp(Upc.input)
Upc.input$cptable[which.min(Upc.input$cptable[,4]),1]
# Minimum Cp value is 10e-04

Upc.input.optimal <- prune(Upc.input, cp = Upc.input$cptable[which.min(Upc.input$cptable[,4]),1])
WalmartCombi$Upc[is.na(WalmartCombi$Upc)] <- predict(Upc.input.optimal, WalmartCombi[is.na(WalmartCombi$Upc), ])

# FinelineNumber NA values

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
set.seed(1455)
FinelineNumber.input <- rpart(FinelineNumber ~ VisitNumber + Weekday + ScanCount + DepartmentDescription, data = WalmartCombi[!is.na(WalmartCombi$FinelineNumber), ], method = "anova", control = rpart.control(minsplit = 10, cp = 0.0001))
stopCluster(cl)
rm(cl)

plotcp(FinelineNumber.input)
FinelineNumber.input$cptable[which.min(FinelineNumber.input$cptable[,4]),1]
# Minimum Cp value is 10e-04

FinelineNumber.input.optimal <- prune(FinelineNumber.input, cp = FinelineNumber.input$cptable[which.min(FinelineNumber.input$cptable[,4]),1])
WalmartCombi$FinelineNumber[is.na(WalmartCombi$FinelineNumber)] <- predict(FinelineNumber.input.optimal, WalmartCombi[is.na(WalmartCombi$FinelineNumber), ])

Train <- head(WalmartCombi, nrow(WalmartTrain))
Test <- tail(WalmartCombi, nrow(WalmartTest))
Test$TripType <- NULL


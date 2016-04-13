library(MASS)
library(randomForest)
library(ggplot2)
setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Matin - Exploration/")
# Loading the daily dataset (baseline)
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d07_newCircadian.Rdata")

# Mobility -----------------------------------------------------------------

classification.df <- newCircadian.df[, c(1:17)]
classification.df[is.na(classification.df)] <- 0
classification.df$class.var <- newCircadian.df$class.mobilityImpaired

# Scaling the data for kNN
scaled.classification.df <- classification.df
for(i in 2:(ncol(scaled.classification.df) - 1)) {
     scaled.classification.df[, i] <- (scaled.classification.df[, i] - min(scaled.classification.df[, i])) / (max(scaled.classification.df[, i]) - min(scaled.classification.df[, i]))
}
rm(i)

# Shuffling for cross validation
set.seed(5855)
shuffled.idx <- sample.int(nrow(classification.df))
classification.df <- classification.df[shuffled.idx, ]
scaled.classification.df <- scaled.classification.df[shuffled.idx, ]
rm(shuffled.idx)

cv.parts <- seq(1, nrow(classification.df), by = ceiling(nrow(classification.df) / 5))
cv.parts <- c(cv.parts, nrow(classification.df))

# QDA ################################
TP <- 0; FP <- 0; TN <- 0; FN <- 0
for(i in 1:(length(cv.parts) - 1)) {
     test.idx <- cv.parts[i]:cv.parts[i+1]
     test.df <- classification.df[test.idx, 2:ncol(classification.df)]
     train.df <- classification.df[-test.idx, 2:ncol(classification.df)]
     fit <- qda(class.var~., data = train.df)
     predictions <- as.character(predict(fit, test.df[, -ncol(test.df)])$class)
     actuals <- test.df$class.var
     tbl <- table(actuals, predictions)
     TP <- TP + tbl[2, 2]
     TN <- TN + tbl[1, 1]
     FP <- FP + tbl[1, 2]
     FN <- FN + tbl[2, 1]
}

rm(i, train.df, test.df, test.idx, fit, predictions, tbl, actuals)
accuracy <- round(((TP + TN) * 100 / nrow(classification.df)), digits = 2)
sensitivity <- round(((TP) * 100 / (TP + FN)), digits = 2)
specificity <- round(((TN) * 100 / (TN + FP)), digits = 2)
print(paste("QDA (Mobility): accuracy(", accuracy, ") - sensitivity (", sensitivity, ") - specificity (", specificity, ")", sep = " "))
rm(accuracy, sensitivity, specificity, TP, TN, FP, FN)

# KNN - scaled dataset used ################################
for(neighbors in c(1, 3, 5, 10, 20, 50)) {
     TP <- 0; FP <- 0; TN <- 0; FN <- 0
     for(i in 1:(length(cv.parts) - 1)) {
          test.idx <- cv.parts[i]:cv.parts[i+1]
          test.df <- scaled.classification.df[test.idx, 2:ncol(scaled.classification.df)]
          train.df <- scaled.classification.df[-test.idx, 2:ncol(scaled.classification.df)]
          predictions <- knn(train = train.df[, -ncol(train.df)], test = test.df[, -ncol(test.df)], cl = train.df[, ncol(train.df)], k = neighbors, prob = F)
          actuals <- as.character(test.df$class.var)
          tbl <- table(actuals, predictions)
          TP <- TP + tbl[2, 2]
          TN <- TN + tbl[1, 1]
          FP <- FP + tbl[1, 2]
          FN <- FN + tbl[2, 1]
     }
     rm(i, train.df, test.df, test.idx, predictions, tbl, actuals)
     accuracy <- round(((TP + TN) * 100 / nrow(classification.df)), digits = 2)
     sensitivity <- round(((TP) * 100 / (TP + FN)), digits = 2)
     specificity <- round(((TN) * 100 / (TN + FP)), digits = 2)
     print(paste("KNN-", neighbors, " (Mobility): accuracy (", accuracy, ") - sensitivity (", sensitivity, ") - specificity (", specificity, ")", sep = " "))
     rm(accuracy, sensitivity, specificity, TP, TN, FP, FN)
}
rm(neighbors)

# Random Forest #############################################
for(number.of.features in c(1, 2)) {
     set.seed(5855)
     fit <- randomForest(class.var~., data = classification.df[, -1], ntree = 100000, mtry = number.of.features, importance = F)
     tbl <- fit$confusion[, 1:2]
     accuracy <- round((tbl[1, 1] + tbl[2, 2]) * 100 / nrow(classification.df), digits = 2)
     sensitivity <- round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2)
     specificity <- round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2)
     print(paste("RF-", number.of.features, " (Mobility): accuracy (", accuracy, ") - sensitivity (", sensitivity, ") - specficitiy (", specificity, ")", sep = " "))
     rm(fit, tbl, accuracy, sensitivity, specificity)
}
rm(number.of.features)


# Cognition Dysfunction -----------------------------------------------------------------
classification.df$class.var <- newCircadian.df$class.cognitionImpaired

# Scaling the data for kNN
scaled.classification.df <- classification.df
for(i in 2:(ncol(scaled.classification.df) - 1)) {
     scaled.classification.df[, i] <- (scaled.classification.df[, i] - min(scaled.classification.df[, i])) / (max(scaled.classification.df[, i]) - min(scaled.classification.df[, i]))
}
rm(i)

# Shuffling for cross validation
set.seed(5855)
shuffled.idx <- sample.int(nrow(classification.df))
classification.df <- classification.df[shuffled.idx, ]
scaled.classification.df <- scaled.classification.df[shuffled.idx, ]
rm(shuffled.idx)

cv.parts <- seq(1, nrow(classification.df), by = ceiling(nrow(classification.df) / 5))
cv.parts <- c(cv.parts, nrow(classification.df))

# QDA ################################
TP <- 0; FP <- 0; TN <- 0; FN <- 0
for(i in 1:(length(cv.parts) - 1)) {
     test.idx <- cv.parts[i]:cv.parts[i+1]
     test.df <- classification.df[test.idx, 2:ncol(classification.df)]
     train.df <- classification.df[-test.idx, 2:ncol(classification.df)]
     fit <- qda(class.var~., data = train.df)
     predictions <- as.character(predict(fit, test.df[, -ncol(test.df)])$class)
     actuals <- test.df$class.var
     tbl <- table(actuals, predictions)
     TP <- TP + tbl[2, 2]
     TN <- TN + tbl[1, 1]
     FP <- FP + tbl[1, 2]
     FN <- FN + tbl[2, 1]
}

rm(i, train.df, test.df, test.idx, fit, predictions, tbl, actuals)
accuracy <- round(((TP + TN) * 100 / nrow(classification.df)), digits = 2)
sensitivity <- round(((TP) * 100 / (TP + FN)), digits = 2)
specificity <- round(((TN) * 100 / (TN + FP)), digits = 2)
print(paste("QDA (Cognition): accuracy(", accuracy, ") - sensitivity (", sensitivity, ") - specificity (", specificity, ")", sep = " "))
rm(accuracy, sensitivity, specificity, TP, TN, FP, FN)

# KNN - scaled dataset used ################################
for(neighbors in c(1, 3, 5, 10, 20, 50)) {
     TP <- 0; FP <- 0; TN <- 0; FN <- 0
     for(i in 1:(length(cv.parts) - 1)) {
          test.idx <- cv.parts[i]:cv.parts[i+1]
          test.df <- scaled.classification.df[test.idx, 2:ncol(scaled.classification.df)]
          train.df <- scaled.classification.df[-test.idx, 2:ncol(scaled.classification.df)]
          predictions <- knn(train = train.df[, -ncol(train.df)], test = test.df[, -ncol(test.df)], cl = train.df[, ncol(train.df)], k = neighbors, prob = F)
          actuals <- as.character(test.df$class.var)
          tbl <- table(actuals, predictions)
          TP <- TP + tbl[2, 2]
          TN <- TN + tbl[1, 1]
          FP <- FP + tbl[1, 2]
          FN <- FN + tbl[2, 1]
     }
     rm(i, train.df, test.df, test.idx, predictions, tbl, actuals)
     accuracy <- round(((TP + TN) * 100 / nrow(classification.df)), digits = 2)
     sensitivity <- round(((TP) * 100 / (TP + FN)), digits = 2)
     specificity <- round(((TN) * 100 / (TN + FP)), digits = 2)
     print(paste("KNN-", neighbors, " (Cognition): accuracy (", accuracy, ") - sensitivity (", sensitivity, ") - specificity (", specificity, ")", sep = " "))
     rm(accuracy, sensitivity, specificity, TP, TN, FP, FN)
}
rm(neighbors)

# Random Forest #############################################
for(number.of.features in c(1, 2)) {
     set.seed(5855)
     fit <- randomForest(class.var~., data = classification.df[, -1], ntree = 100000, mtry = number.of.features, importance = F)
     tbl <- fit$confusion[, 1:2]
     accuracy <- round((tbl[1, 1] + tbl[2, 2]) * 100 / nrow(classification.df), digits = 2)
     sensitivity <- round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2)
     specificity <- round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2)
     print(paste("RF-", number.of.features, " (Cognition): accuracy (", accuracy, ") - sensitivity (", sensitivity, ") - specficitiy (", specificity, ")", sep = " "))
     rm(fit, tbl, accuracy, sensitivity, specificity)
}
rm(number.of.features)


# Cardio Vascular Disease -----------------------------------------------------------------
classification.df$class.var <- newCircadian.df$class.cvd

# Scaling the data for kNN
scaled.classification.df <- classification.df
for(i in 2:(ncol(scaled.classification.df) - 1)) {
     scaled.classification.df[, i] <- (scaled.classification.df[, i] - min(scaled.classification.df[, i])) / (max(scaled.classification.df[, i]) - min(scaled.classification.df[, i]))
}
rm(i)

# Shuffling for cross validation
set.seed(5855)
shuffled.idx <- sample.int(nrow(classification.df))
classification.df <- classification.df[shuffled.idx, ]
scaled.classification.df <- scaled.classification.df[shuffled.idx, ]
rm(shuffled.idx)

cv.parts <- seq(1, nrow(classification.df), by = ceiling(nrow(classification.df) / 5))
cv.parts <- c(cv.parts, nrow(classification.df))

# QDA ################################
TP <- 0; FP <- 0; TN <- 0; FN <- 0
for(i in 1:(length(cv.parts) - 1)) {
     test.idx <- cv.parts[i]:cv.parts[i+1]
     test.df <- classification.df[test.idx, 2:ncol(classification.df)]
     train.df <- classification.df[-test.idx, 2:ncol(classification.df)]
     fit <- qda(class.var~., data = train.df)
     predictions <- as.character(predict(fit, test.df[, -ncol(test.df)])$class)
     actuals <- test.df$class.var
     tbl <- table(actuals, predictions)
     TP <- TP + tbl[2, 2]
     TN <- TN + tbl[1, 1]
     FP <- FP + tbl[1, 2]
     FN <- FN + tbl[2, 1]
}

rm(i, train.df, test.df, test.idx, fit, predictions, tbl, actuals)
accuracy <- round(((TP + TN) * 100 / nrow(classification.df)), digits = 2)
sensitivity <- round(((TP) * 100 / (TP + FN)), digits = 2)
specificity <- round(((TN) * 100 / (TN + FP)), digits = 2)
print(paste("QDA (CVD): accuracy(", accuracy, ") - sensitivity (", sensitivity, ") - specificity (", specificity, ")", sep = " "))
rm(accuracy, sensitivity, specificity, TP, TN, FP, FN)

# KNN - scaled dataset used ################################
for(neighbors in c(1, 3, 5, 10, 20, 50)) {
     TP <- 0; FP <- 0; TN <- 0; FN <- 0
     for(i in 1:(length(cv.parts) - 1)) {
          test.idx <- cv.parts[i]:cv.parts[i+1]
          test.df <- scaled.classification.df[test.idx, 2:ncol(scaled.classification.df)]
          train.df <- scaled.classification.df[-test.idx, 2:ncol(scaled.classification.df)]
          predictions <- knn(train = train.df[, -ncol(train.df)], test = test.df[, -ncol(test.df)], cl = train.df[, ncol(train.df)], k = neighbors, prob = F)
          actuals <- as.character(test.df$class.var)
          tbl <- table(actuals, predictions)
          TP <- TP + tbl[2, 2]
          TN <- TN + tbl[1, 1]
          FP <- FP + tbl[1, 2]
          FN <- FN + tbl[2, 1]
     }
     rm(i, train.df, test.df, test.idx, predictions, tbl, actuals)
     accuracy <- round(((TP + TN) * 100 / nrow(classification.df)), digits = 2)
     sensitivity <- round(((TP) * 100 / (TP + FN)), digits = 2)
     specificity <- round(((TN) * 100 / (TN + FP)), digits = 2)
     print(paste("KNN-", neighbors, " (CVD): accuracy (", accuracy, ") - sensitivity (", sensitivity, ") - specificity (", specificity, ")", sep = " "))
     rm(accuracy, sensitivity, specificity, TP, TN, FP, FN)
}
rm(neighbors)

# Random Forest #############################################
for(number.of.features in c(1, 2)) {
     set.seed(5855)
     fit <- randomForest(class.var~., data = classification.df[, -1], ntree = 100000, mtry = number.of.features, importance = F)
     tbl <- fit$confusion[, 1:2]
     accuracy <- round((tbl[1, 1] + tbl[2, 2]) * 100 / nrow(classification.df), digits = 2)
     sensitivity <- round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2)
     specificity <- round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2)
     print(paste("RF-", number.of.features, " (CVD): accuracy (", accuracy, ") - sensitivity (", sensitivity, ") - specficitiy (", specificity, ")", sep = " "))
     rm(fit, tbl, accuracy, sensitivity, specificity)
}
rm(number.of.features)


rm(list = ls())


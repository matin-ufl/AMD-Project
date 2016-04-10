set.seed(1234)
tr <- trainControl(method="repeatedcv",repeats = 20, number=5)

daily.cvd.qda <- train(class.cvd ~ ., trControl = tr, data = daily.df[,c(2:7,13)], method="qda")

set.seed(1234)
daily.cvd.tree <- train(class.cvd ~ ., trControl = tr, data = daily.df[,c(2:7,13)], method="rpart")

set.seed(1234)
daily.cvd.rf <- train(class.cvd ~ ., trControl = tr, data = daily.df[,c(2:7,13)], method="rf",ntree=10000)

set.seed(1234)
daily.cvd.knn <- train(class.cvd ~ ., trControl = tr, data = daily.df[,c(2:7,13)], method="knn",tuneGrid=expand.grid(k=1:10))

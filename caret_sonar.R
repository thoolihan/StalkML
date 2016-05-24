library(mlbench)
library(caret)

data(Sonar)
set.seed(1)

inTrain <- createDataPartition(y = Sonar$Class, p = 0.75, list = FALSE)

training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

model <- train(Class ~ .,
               data = training,
               method = "pls",
               tuneLength = 15,
               trControl = ctrl,
               metric = "ROC",
               preProc = c('center', 'scale'))

print(model)
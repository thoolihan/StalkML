library(plyr)
library(dplyr)
library(caret)

market.classes <- c(rep('numeric', 14), 'factor')
market <- read.csv("data/stalk.csv", 
                   colClasses = market.classes, 
                   na.strings = c('', 'NA'),
                   header = TRUE)

market <- dplyr::select(market, -WedAM, -WedPM, -ThuAM, -ThuPM,
                        -FriAM, -FriPM, -SatAM, -SatPM, -Week) %>%
  mutate(ch0 = MonAM - SunAM,
         ch1 = MonPM - MonAM,
         ch2 = TueAM - MonPM,
         ch3 = TuePM - TueAM,
         EvenStart = SunAM %% 2) %>%
  na.omit()

market$IsDescending <- revalue(market$IsDescending, c(Y = 'YES', N = 'NO'))
          
train.rows <- createDataPartition(market$IsDescending, p = 0.6, list = FALSE)
data.train <- market[train.rows,]
data.test <- market[-(train.rows),]

train.ctrl <- trainControl(method = "repeatedcv", 
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
tg <- expand.grid(.mtry = 1:5)

model <- train(IsDescending ~ .,
               data = data.train,
               method = "rf",
               trControl = train.ctrl,
               tuneGrid = tg,
               metric = "ROC",
               preProcess = c('center', 'scale'))

print(model)

data.test$Output <- predict(model, data.test)
print(confusionMatrix(data.test$Output, data.test$IsDescending))

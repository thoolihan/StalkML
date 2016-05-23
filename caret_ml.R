library(plyr)
library(dplyr)
library(caret)

market.classes <- c(rep('numeric', 14), 'factor')
market <- read.csv("data/stalk.csv", 
                   #colClasses = market.classes, 
                   na.strings = c('', 'NA'),
                   header = TRUE)

market <- dplyr::select(market, -WedAM, -WedPM, -ThuAM, -ThuPM,
                        -FriAM, -FriPM, -SatAM, -SatPM, -Week) %>%
#   mutate(ch0 = MonAM - SunAM,
#          ch1 = MonPM - MonAM,
#          ch2 = TueAM - MonPM,
#          ch3 = TuePM - TueAM,
#          EvenStart = SunAM %% 2) %>%
  na.omit() %>%
  mutate(IsDescending = factor(IsDescending))

market$IsDescending <- revalue(market$IsDescending, c(Y = 'YES', N = 'NO'))
          
train.rows <- createDataPartition(market$IsDescending, p = 0.6, list = FALSE)
data.train <- market[train.rows,]
data.test <- market[-(train.rows),]

train.ctrl <- trainControl(method = "repeatedcv", 
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

model <- train(IsDescending ~ .,
               data = data.train,
               method = "pls",
               tuneLength = 15,
               trControl = train.ctrl,
               metric = "ROC",
               preProcess = c('center', 'scale'))

#print(summary(model))
#print(anova(model, test = "Chisq"))

#data.test$Prob <- predict.glm(model, data.test, type = 'response')
#data.test$Output <- ifelse((data.test$Prob >= 0.5), 'Y', 'N')
#print(confusionMatrix(data.test$Output, data.test$IsDescending))

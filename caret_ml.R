library(ggplot2)
library(plyr)
library(dplyr)
library(foreach)
library(caret)

market.classes <- c(rep('numeric', 14), 'factor')
market <- read.csv("data/stalk.csv", 
                   colClasses = market.classes, 
                   na.strings = c('', 'NA'),
                   header = TRUE)

market <- dplyr::select(market, -WedAM, -WedPM, -ThuAM, -ThuPM,
                        -FriAM, -FriPM, -SatAM, -SatPM, -Week) %>%
  mutate(SunAM = ifelse(is.na(SunAM), mean(SunAM, na.rm = TRUE), SunAM),
         ch0 = MonAM - SunAM,
         ch1 = MonPM - MonAM,
         ch2 = TueAM - MonPM,
         ch3 = TuePM - TueAM) %>%
  na.omit() 
          
train.rows <- createDataPartition(market$IsDescending, p = 0.6, list = FALSE)
data.train <- market[train.rows,]
data.test <- market[-(train.rows),]

train.ctrl <- trainControl(summaryFunction = twoClassSummary(),
                        classProbs = TRUE)

model <- train(IsDescending ~ .,
               data = data.train,
               method = "glm",
               metric = "rmse", 
               trControl = train.ctrl)

print(summary(model))
print(anova(model, test = "Chisq"))

data.test$Prob <- predict.glm(model, data.test, type = 'response')
data.test$Output <- ifelse((data.test$Prob >= 0.5), 'Y', 'N')
print(confusionMatrix(data.test$Output, data.test$IsDescending))

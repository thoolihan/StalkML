library(ggplot2)
library(plyr)
library(dplyr)
library(foreach)
library(caret)

market.classes <- c(rep('integer', 14), 'factor')
market <- read.csv("data/stalk.csv", 
                   colClasses = market.classes, 
                   na.strings = c('', 'NA'),
                   header = TRUE)

market <- select(market, -WedAM, -WedPM, -ThuAM, -ThuPM,
                 -FriAM, -FriPM, -SatAM, -SatPM, -Week) %>%
          mutate(SunAM = as.integer(ifelse(is.na(SunAM), mean(SunAM, na.rm = TRUE), SunAM))) %>%
          na.omit() 
          
train.rows <- createDataPartition(market$IsDescending, p = 0.6, list = FALSE)
data.train <- market[train.rows,]
data.test <- market[-(train.rows),]

train.ctrl <- trainControl(method = "repeatedcv", 
                           repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
model <- glm(IsDescending ~ ., 
             data = data.train,
             family = binomial("logit"))

# model <- train(IsDescending ~ .,
#                data = data.train,
#                method = "glm",
#                metric = "ROC", 
#                trControl = train.ctrl)
print(summary(model))
print(anova(model, test = "Chisq"))

data.test$Prob <- predict.glm(model, data.test, type = 'response')
data.test$Output <- ifelse((data.test$Prob >= 0.5), 'Y', 'N')
print(confusionMatrix(data.test$Output, data.test$IsDescending))

library(xgboost)
library(plyr)
library(dplyr)
library(caret)

# load data
market.classes <- c(rep('numeric', 14), 'character')
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
         ch3 = TuePM - TueAM,
         EvenStart = SunAM %% 2,
         IsDescending = as.integer(revalue(IsDescending, replace = c('Y' = 1, 'N' = 0)))
         ) %>%
  na.omit() 

train.rows <- createDataPartition(market$IsDescending, p = 0.6, list = FALSE)
data.train <- market[train.rows,]
data.test <- market[-(train.rows),]

# create model
feature_matrix <- function(df) { dplyr::select(df, -IsDescending) %>% data.matrix()}

model <- xgboost(data = feature_matrix(data.train), 
                 label = data.train$IsDescending, 
                 max.depth = 5, nthread = 4, nround = 10 , 
                 objective = "binary:logistic")

# predict
data.test$Prob <- xgboost::predict(model, feature_matrix(data.test))
data.test$Output <- round(data.test$Prob, 0)

results <- confusionMatrix(data.test$Output, data.test$IsDescending)
print(results)
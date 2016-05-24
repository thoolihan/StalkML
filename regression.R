library(ggplot2)
library(dplyr)
library(foreach)
library
library(caret)

# based on: http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# load data
market <- read.csv("data/stalk.csv", 
                   colClasses = c(rep('numeric', 14), 'factor'),
                   header = TRUE)

# alter columns
market <- mutate(market,
                 ch0 = MonAM - SunAM,
                 ch1 = MonPM - MonAM,
                 ch2 = TueAM - MonPM,
                 ch3 = TuePM - TueAM,
                 even_start = SunAM %% 2) %>%
  na.omit()

# normalize
normalize <- function(col) {
  return ((col - min(col)) / (max(col) - min(col)))
}

for(col in c('SunAM', 'MonAM', 'MonPM', 'TueAM', 'TuePM', 
             'ch0', 'ch1', 'ch2', 'ch3')) { 
  market[, col] <-  normalize(market[, col])
}

# split
tri <- createDataPartition(market$IsDescending, p = 0.65, list = FALSE)
train <- market[tri,]
test <- market[-tri,]

# create model

model <- glm(IsDescending ~ SunAM * MonAM * MonPM * TueAM * TuePM *
               ch0 * ch1 * ch2 * even_start, 
             family=binomial(link='logit'), 
             data = train,
             control = list(maxit = 500))

# predict
test$prob <- predict(model, test)
test$output <- factor(ifelse(test$prob > 0.5, 'Y', 'N'), 
                      levels = levels(test$IsDescending))
print(confusionMatrix(test$output, test$IsDescending))


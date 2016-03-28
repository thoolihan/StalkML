library(ggplot2)

# based on: http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# load data
market <- read.csv("data/stalk.csv", header = TRUE)
factors <- c("SunAM", "MonAM", "MonPM", "TueAM", "TuePM", "ch0", "ch1", "ch2", "ch3")
keep <- c(factors, "IsDescending")

# alter columns
market$ch0 <- market$MonAM - market$SunAM
market$ch1 <- market$MonPM - market$MonAM
market$ch2 <- market$TueAM - market$MonPM
market$ch3 <- market$TuePM - market$TueAM
market$IsDescending <- as.integer(ifelse(market$IsDescending == 'Y', 1, 0))

# clean up
market <- market[, keep]
market <- na.omit(market)

# normalize
normalize <- function(col) {
  return ((col - min(col)) / (max(col) - min(col)))
}

for(col in factors) { market[, col] <-  normalize(market[, col])}

# helpful variables
n <- nrow(market)
split <- floor(n * 0.7)
itrain <- 1:split
itest <- (split + 1):n

# randomize
market <- market[sample(n),]

# split
train <- market[itrain,]
test <- market[itest,]

# create model
model <- glm(IsDescending ~ ., 
             family=binomial(link='logit'), 
             data = train)

# predict
test$prediction <- predict(model, newdata = test[, factors], type='response')
test$prediction <- ifelse(test$prediction > 0.5, 1, 0)

# setup rsults
test$tp <- ifelse(test$prediction == 1 & test$IsDescending == 1, 1, 0)
test$fp <- ifelse(test$prediction == 1 & test$IsDescending == 0, 1, 0)
test$tn <- ifelse(test$prediction == 0 & test$IsDescending == 0, 1, 0)
test$fn <- ifelse(test$prediction == 0 & test$IsDescending == 1, 1, 0)

# accuracy
acc <- (sum(test$tp) + sum(test$tn)) / nrow(test)
print(paste('Accuracy: ', acc))

# precision (tp / tp + fp)
precision <- sum(test$tp) / (sum(test$tp) + sum(test$fp))
if(is.na(precision) | precision == 0) { precision <- 0.001 }
print(paste('Precision: ', precision))

# recall (tp / tp + fn)
recall <- sum(test$tp) / (sum(test$tp) + sum(test$fn))
if(is.na(recall) | recall == 0) { recall <- 0.001 }
print(paste('Recall: ', recall))

f1 <- 2 * (precision * recall / (precision + recall))
print(paste('F1 Score: ', f1))
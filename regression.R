library(ggplot2)

# based on: http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# load data
market <- read.csv("data/stalk.csv", header = TRUE)
factors <- c("MonAM", "MonPM", "TueAM", "TuePM")
keep <- c(factors, "IsDescending")
market <- market[, keep]

# clean up
market$IsDescending <- as.integer(ifelse(market$IsDescending == 'Y', 1, 0))
market <- na.omit(market)

# helpful variables
n <- nrow(market)
split <- floor(n * 0.7)
itrain <- 1:split
itest <- (split + 1):n

# randomize
market <- market[sample(n),]

train <- market[itrain,]
test <- market[itest,]

# create model
model <- glm(IsDescending ~.,family=binomial(link='logit'),data=train)

# predict
test$prediction <- predict(model,newdata=test[,factors],type='response')
test$prediction <- ifelse(test$prediction > 0.5,1,0)

# setup rsults
test$tp <- ifelse(test$prediction == 1 & test$IsDescending == 1, 1, 0)
test$fp <- ifelse(test$prediction == 1 & test$IsDescending == 0, 1, 0)
test$tn <- ifelse(test$prediction == 0 & test$IsDescending == 0, 1, 0)
test$fn <- ifelse(test$prediction == 0 & test$IsDescending == 1, 1, 0)

# accuracy
acc <- (sum(test$tp) + sum(test$tn)) / nrow(test)
print(paste('Accuracy: ',acc))

# precision (tp / tp + fp)
precision <- sum(test$tp) / (sum(test$tp) + sum(test$fp))
print(paste('Precision',precision))

# recall (tp / tp + fn)
recall <- sum(test$tp) / (sum(test$tp) + sum(test$fn))
print(paste('Recall: ',recall))

f1 <- 2 * (precision * recall / (precision + recall))
print(paste('F1 Score: ', f1))
library(ggplot2)
library(functional)
source('sigmoid.R')

# load data
market <- read.csv("data/stalk.csv", header = TRUE)
factors <- c("bias", "SunAM", "MonAM", "MonPM", "TueAM", "TuePM", "ch0", "ch1", "ch2", "ch3")
normalized_factors <- factors[!(factors %in% c("bias"))]
keep <- c(factors, "IsDescending")

# alter columns
market$bias <- 1
market$ch0 <- market$MonAM - market$SunAM
market$ch1 <- market$MonPM - market$MonAM
market$ch2 <- market$TueAM - market$MonPM
market$ch3 <- market$TuePM - market$TueAM
market$IsDescending <- as.integer(ifelse(market$IsDescending == 'Y', 1, 0))

# clean up
market <- market[, keep]
market <- na.omit(market)

# normalize
normalize <- function(col, name = deparse(substitute(col))) {
  print(paste('normalizing ', name, ' mean:', mean(col), ' sd:', sd(col)))
  return ((col - mean(col)) / sd(col))
}

for(col in normalized_factors) { market[, col] <-  normalize(market[, col], col)}

#add bias column
market$bias <- 1

# helpful variables
n <- nrow(market)
split <- floor(n * 0.6)
itrain <- 1:split

# randomize
market <- market[sample(n),]

# split
train <- market[itrain,]
test <- market[-itrain,]

# create model
X <- data.matrix(market[,factors])
y <- market$IsDescending
theta <- rep(0, length(factors))
lambda <- 0.25

cost <- function(theta, X, y, lambda) {
  vals <- X %*% theta
  (1 / nrow(X)) * sum((-1 * y) * log(sigmoid(vals)) - 
                  (1 - y) * log(1 - sigmoid(vals))) +
      ((lambda / nrow(X)) * sum(theta[2:length(theta)] ^ 2))
}

cost_wrapper <- Curry(cost, X = X, y = y, lambda = lambda)

grad <- function(theta, X, y, lambda) {
  vals <- X %*% theta
  (1 / nrow(X)) * (t(X) %*% (sigmoid(vals) - y)) + 
    ((lambda / nrow(X)) * sum(theta[2:length(theta)]))
}

grad_wrapper <- Curry(grad, X = X, y = y, lambda = lambda)

model <- optim(par = theta, 
               fn = cost_wrapper, 
               gr = grad_wrapper,
               control = list(maxit = 1000))
theta <- model$par
names(theta) <- factors 

# predict
X2 <- data.matrix(test[, factors])
predict <- function(cases, params) {
  round(sigmoid(cases %*% params), 0)
}
test$prediction <- predict(X2, theta)

# setup rsults
test$tp <- ifelse(test$prediction == 1 & test$IsDescending == 1, 1, 0)
test$fp <- ifelse(test$prediction == 1 & test$IsDescending == 0, 1, 0)
test$tn <- ifelse(test$prediction == 0 & test$IsDescending == 0, 1, 0)
test$fn <- ifelse(test$prediction == 0 & test$IsDescending == 1, 1, 0)

print(theta)

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
library(xgboost)
library(dplyr)

# load data
market <- read.csv("data/stalk.csv", header = TRUE)
factors <- c("bias", "SunAM", "MonAM", "MonPM", "TueAM", "TuePM", "ch0", "ch1", "ch2", "ch3")
normalized_factors <- factors[!(factors %in% c("bias"))]
keep <- c(factors, "IsDescending")

# alter columns
market <- mutate(market, 
       bias = 1,
       ch0 = MonAM - SunAM,
       ch1 = MonPM - MonAM,
       ch2 = TueAM - MonPM,
       ch3 = TuePM - TueAM,
       IsDescending = as.integer(ifelse(IsDescending == 'Y', 1, 0))
       )

# clean up
market <- market[, keep]
market <- na.omit(market)

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

model <- xgboost(data = X, label = y, 
                    max.depth = 2, nthread = 4, nround = 2, 
                    objective = "binary:logistic")

# predict
test$predicted <- round(xgboost::predict(model, data.matrix(test)), 0)

tp <- sum(test$IsDescending == 1 & test$predicted == 1)
tn <- sum(test$IsDescending == 0 & test$predicted == 0)
fp <- sum(test$IsDescending == 0 & test$predicted == 1)
fn <- sum(test$IsDescending == 1 & test$predicted == 0)

results <- list(
  acc = round((tp + tn) / nrow(test), 3),
  prec = round(tp / (tp + fp), 3),
  rec = round(tp / (fn + tp), 3)
)
results$f1 = round(2 * results$prec * results$rec / (results$prec + results$rec), 3)

print(results)
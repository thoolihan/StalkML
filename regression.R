library(ggplot2)

# load data
market <- read.csv("data/stalk.csv", header = TRUE)

keep <- c("MonAM", "MonPM", "TueAM", "TuePM", "IsDescending")
market <- market[, keep]

# clean up
market$IsDescending <- as.integer(ifelse(market$IsDescending == 'Y', 1, 0))

na.omit(market)

# helpful variables
n <- nrow(market)
split <- floor(n * 0.7)
itrain <- 1:split
itest <- (split + 1):n

# randomize
market <- market[sample(n),]

train <- market[itrain,]
test <- market[itest,]

model <- glm(IsDescending ~.,family=binomial(link='logit'),data=train)
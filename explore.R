# load data
market <- read.csv("stalk.csv", header = TRUE)

# clean up
market <- na.omit(market)
market$IsDescending <- ifelse(market$IsDescending == 'Y', 1, 0)
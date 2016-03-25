library(ggplot2)

# load data
market <- read.csv("data/stalk.csv", header = TRUE)

# clean up
market <- na.omit(market)
market$IsDescending <- ifelse(market$IsDescending == 'Y', 1, 0)

# average loss of a Thursday sale vs Tuesday sale for descending
pos <- market[market$IsDescending == 1,]
avg_loss <- mean(pos$TuePM - pos$ThuPM)
print(paste("tu - th loss", avg_loss))

# plot descending by starting price
print(qplot(market$IsDescending, market$SunAM))
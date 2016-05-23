library(ggplot2)

# load data
market <- read.csv("data/stalk.csv", 
                   colClasses = c(rep('numeric', 14), 'factor'), 
                   header = TRUE)

# average loss of a Thursday sale vs Tuesday sale for descending
pos <- market[market$IsDescending == 'Y',]
avg_loss <- mean(pos$TuePM - pos$ThuPM, na.rm = TRUE)
print(paste("Average Loss When Descending", avg_loss))

# plot
plt <- ggplot(market, 
              aes(SunAM, fill = factor(IsDescending))) + 
        geom_histogram(binwidth = 1)
print(plt)



## PART ONE

library(dplyr)
library(tidyr)

data <- read.csv(unz("activity.zip", "activity.csv"))

sorted_data <- arrange(data, date, interval) %>% select(date, interval, steps)

clean_data <- sorted_data[complete.cases(sorted_data),]

temp <- clean_data$interval
temp <- sprintf("%04d", temp)
clean_data$interval <- format(strptime(temp, format="%H%M"), format = "%H:%M")
remove(temp)

sums <- clean_data %>% group_by(date) %>% summarize(steps = sum(steps))
hist(sums$steps, xlab = "Steps per Day", ylab = "Number of Days", main = "Frequency of Total Steps over Time",
     breaks = 8)

partone <- sums %>% summarize(mean = mean(steps), median = median(steps))

## PART TWO
?ggplot
library(ggplot2)
?plot
plot(sorted_data$interval ~ mean(sorted_data$steps))


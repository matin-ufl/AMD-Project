library(randomForest)
library(ggplot2)
library(reshape2)

# Functions ---------------------------------------------
convert.time.to.string <- function(hour) {
     result <- paste(hour, ":00", sep = "")
     result
}

plot.log.values <- function(PID, participant.1h = participant.1h) {
     g <- ggplot(data = participant.1h) + geom_line(aes(x = hour, y = log.activity.count_hourly, group = "a"), colour = "blue", size = 1)
     g <- g + theme_bw() + labs(title = PID, x = "Hour of day", y = "Log Activity Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
     g <- g + geom_hline(yintercept = log2(100*60), colour = "red") + annotate("text", x = 2, y = log2(100*60) + 0.5, label = "Cutoff = 100 count/min", size = 5)
     print(g)
}

# Script -------------------------------------------------
setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")

minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - wear time only - 04_06_16/"
outFolder <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer hour data - wear time only - 04_06_16/"
l <- dir(minute_directory)
for(f in 1:length(l)) {
     load(paste(minute_directory, l[f], sep = ""))
     participant.1h <- data.frame(matrix(nrow = 24, ncol = 5))
     colnames(participant.1h) <- c("hour", "activity.count_sum", "minutes_sum", "activity.count_hourly", "log.activity.count_hourly")
     for(i in 0:23) {
          participant.1h$hour[i + 1] <- convert.time.to.string(i)
          participant.1h$activity.count_sum[i + 1] <- sum(participant.1m$VM[participant.1m$hour.of.day == i])
          participant.1h$minutes_sum[i + 1] <- length(which(participant.1m$hour.of.day == i))
          participant.1h$activity.count_hourly[i + 1] <- participant.1h$activity.count_sum[i + 1] / (participant.1h$minutes_sum[i + 1] / 60)
          if(is.na(participant.1h$activity.count_hourly[i + 1])) {
               participant.1h$activity.count_hourly[i + 1] <- 0
          }
          participant.1h$log.activity.count_hourly[i + 1] <- log2(participant.1h$activity.count_hourly[i + 1])
     }
     
     participant.1h$hour <- factor(participant.1h$hour, levels = participant.1h$hour)
     save(participant.1h, file = paste(outFolder, l[f], sep = ""))
     #plot.log.values(l[f], participant.1h)
}
rm(list = ls())




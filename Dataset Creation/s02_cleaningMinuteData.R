library(ggplot2)


# Functions -------------------------------------------------------

initialize.day.variables <- function() {
     result <- data.frame(matrix(nrow = 0, ncol = 15))
     colnames(result) <- c("08:00", "09:00", "10:00", "11:00", "12:00",
                           "13:00", "14:00", "15:00", "16:00", "17:00",
                           "18:00", "19:00", "20:00", "day.begin", "day.end")
     result
}

find.wearTime.exludeOutlier <- function(AC.1m, PID) {
     data1m = wearingMarking(dataset = AC.1m,
                             frame = 90, 
                             perMinuteCts = 1,
                             TS = "TimeStamp",
                             cts = "VM", 
                             streamFrame = NULL, 
                             allowanceFrame= 2, 
                             newcolname = "wearing")
     
     # First step in defining outliers
     data1m$wearing[data1m$VM > 100000] <- "nw"
     data1m$wearing[data1m$steps > 180] <- "nw"
     
     # Second step: more complicated
     secondHighest.all <- data.frame(day = 1:max(data1m$days), minute.AC = rep(NA, max(data1m$days)))
     for (day in 1:max(data1m$days)) {
          dayStart.idx <- min(which(data1m$days == day))
          if(length(dayStart.idx) > 0) {
               dayEnd.idx <- max(which(data1m$days == day))
               if(length(dayEnd.idx) > 0) {
                    max.ac <- max(data1m$VM[dayStart.idx:dayEnd.idx])
                    valid.idx <- which(data1m$VM[dayStart.idx:dayEnd.idx] < max.ac)
                    if(length(valid.idx) > 0) {
                         valid.idx <- valid.idx + dayStart.idx - 1
                         secondHighest.all$minute.AC[day] <- max(data1m$VM[valid.idx])
                    }
               }
          }
     }
     
     # Now we can have both thresholds calculated: <"median + 3500" and "day + 1000">
     threshold.all <- median(secondHighest.all$minute.AC) + 3500
     for(day in 1:max(data1m$days)) {
          threshold.day <- secondHighest.all$minute.AC[day] + 1000
          dayStart.idx <- min(which(data1m$days == day))
          if(length(dayStart.idx) > 0) {
               dayEnd.idx <- max(which(data1m$days == day))
               if(length(dayEnd.idx) > 0) {
                    candidate.idx <- which(data1m$VM[dayStart.idx:dayEnd.idx] > threshold.all)
                    if(length(candidate.idx) > 0) {
                         candidate.idx <- candidate.idx + dayStart.idx - 1
                         outlier.idx <- which(data1m$VM[candidate.idx] > threshold.day)
                         if(length(outlier.idx) > 0) {
                              data1m$wearing[(outlier.idx + candidate.idx - 1)] <- "nw"
                         }
                    }
               }
          }
     }
     a <- sumVct(data1m, id = PID)
}

clr <- function() {cat(rep("\n", 50))}

timeOfDay <- function(time.str) {
     str <- unlist(strsplit(time.str, split = " "))[2]
     str <- unlist(strsplit(str, split = ":"))[1]
     as.numeric(str)
}

plotting.participant.daily.activity <- function(participant.1m) {
     AC <- rep(0, 24)
     for(h in 1:24) {
          AC[h] <- mean(participant.1m$VM[participant.1m$hour.of.day == (h - 1)], na.rm = T)
     }
     plot.df <- data.frame(AC)
     g <- ggplot(data = plot.df) + geom_line(aes(x = 0:23, y = AC), group = "a", colour = "blue") + theme_bw() + labs(title = PID, y = "Activity Count") + 
          geom_hline(yintercept = 100, colour = "red") + annotate("text", 2, 130, label = "Cutoff", size = 5)
     print(g)
     plot.df
}

# Removing non wears completely -------------------------------------------------------

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - 03_28_16/LIFE accelerometer data - minute data - 03_28_16/"
l <- list.files(path = minute_directory)


for(i in 102:length(l)) {
     
     fileName <- l[i]
     PID <- substr(fileName, 2, 8)
     clr()
     print(paste(i, " out of 1212 - (", PID, ")", sep = ""))
     load(paste(minute_directory, fileName, sep = ""))     
     # First wear times are found
     wear.times <- find.wearTime.exludeOutlier(AC.1m, PID)
     participant.1m <- data.frame(matrix(nrow = 0, ncol = ncol(AC.1m) + 2))
     colnames(participant.1m) <- c(colnames(AC.1m), "day", "hour.of.day")
     valid.day <- 0
     for(currDay in 1:nrow(wear.times)) {
          # The day should have more than 10 hours of valid wear time
          if(wear.times$duration[currDay] > 599) {
               # The rest of the data should be pruned
               valid.day <- valid.day + 1
               hour.of.day <- apply(X = as.matrix(as.character(AC.1m$TimeStamp[wear.times$start[currDay]:wear.times$end[currDay]])), FUN = timeOfDay, MARGIN = 1)
               participant.1m <- rbind(participant.1m, cbind(AC.1m[wear.times$start[currDay]:wear.times$end[currDay], ], valid.day, hour.of.day))
          }
     }
     # Saving "just" the wear time part of participants
     save(participant.1m, file = paste("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - wear time only - 04_06_16/", PID, ".Rdata", sep = ""))
     
}
rm(list = ls())


# Cleaning dataset for feature construction -------------------------------------------------








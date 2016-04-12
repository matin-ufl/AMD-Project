library(PhysicalActivity)
library(stats)

# Functions -----------------------------------------

cosine.fit <- function(VM.ac.w) {
     ssp <- spectrum(VM.ac.w) # find the spectrums of the signal
     per <- 1/ssp$freq[ssp$spec == max(ssp$spec)] # find the frequency where the peak happens
     t <- 1:length(VM.ac.w)
     reslm <- lm(VM.ac.w ~ sin(2*pi/per*t) + cos(2*pi/per*t))
     result <- list(frequency = per, fitted.lm = reslm)
     result
}

convert.fft <- function(cs, sample.rate = 1) {
     cs <- cs / length(cs) # normalize
     
     distance.center <- function(c) signif(Mod(c), 4)
     angle           <- function(c) signif(180 * Arg(c) / pi, 3)
     
     df <- data.frame(cycle    = 0:(length(cs)-1),
                      freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                      strength = sapply(cs, distance.center),
                      delay    = sapply(cs, angle))
     df
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

# Script --------------------------------------------
setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - 03_28_16/LIFE accelerometer data - minute data - 03_28_16/"
l <- list.files(path = minute_directory)


for(i in 244:length(l)) {
     
     fileName <- l[i]
     PID <- substr(fileName, 2, 8)
     clr()
     print(paste(i, " out of 1212 - (", PID, ")", sep = ""))
     load(paste(minute_directory, fileName, sep = ""))     
     # First wear times are found
     wear.times <- find.wearTime.exludeOutlier(AC.1m, PID)
     
     participant.1m <- data.frame(TimeStamp = as.character(AC.1m$TimeStamp),
                                  axis1 = rep(-5, nrow(AC.1m)),
                                  axis2 = rep(-5, nrow(AC.1m)),
                                  axis3 = rep(-5, nrow(AC.1m)),
                                  VM = rep(-5, nrow(AC.1m)),
                                  steps = rep(-5, nrow(AC.1m)))
     for(j in 1:nrow(wear.times)) {
          if(wear.times$duration[j] > 599) {
               participant.1m[wear.times$start[j]:wear.times$end[j], ] <- AC.1m[wear.times$start[j]:wear.times$end[j], ]
          }
     }
     save(participant.1m, file = paste("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - nonwear replaced by negative - 04_11_16/", PID, ".Rdata", sep = ""))
}

rm(list = ls())






load(file.choose())

VM.ac.w <- participant.1m$VM[5362:(5362+714)]
VM.ac.w <- log10(VM.ac.w)
VM.ac.w[VM.ac.w < 0] <- 0
VM.ac.w[VM.ac.w == Inf] <- 0
fitted.cosine <- cosine.fit(VM.ac.w)

avg.log.ac <- rep(0, length(VM.ac.w))
for(i in seq(1, length(VM.ac.w), by = 60)) {
     avg.log.ac[i:(i+59)] <- mean(VM.ac.w[i:(i+59)])
}

rm(i)
plot.df <- data.frame(x = 1:length(VM.ac.w), log.ac = VM.ac.w, avg.log.ac = avg.log.ac[1:length(VM.ac.w)], fitted.ac = fitted.cosine$fitted.lm$fitted.values)
a1 <- melt(data = plot.df[, -3], id.vars = 1)
g <- ggplot(data = a1) + geom_line(aes(x = x, y = value, group = variable, colour = variable, size = variable))
g + scale_colour_manual(values = c("red", "blue")) + scale_size_manual(values = c(1, 2)) + theme_classic()

a2 <- melt(data = plot.df[, -2], id.vars = 1)
g <- ggplot(data = a2) + geom_line(aes(x = x, y = value, group = variable, colour = variable, size = variable))
g + scale_colour_manual(values = c("red", "blue")) + scale_size_manual(values = c(1, 2)) + theme_classic()

g <- ggplot(data = plot.df) + geom_point(aes(x = x, y = log.ac), colour = "red") + geom_line(aes(x = x, y = fitted.ac, group = "fitted"), colour = "blue", size = 2)
g + theme_classic()

library(ggplot2)
library(haven)

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

plotting.participant.daily.activity <- function(participant.1m, PID) {
     AC <- rep(0, 24)
     for(h in 1:24) {
          AC[h] <- mean(participant.1m$VM[participant.1m$hour.of.day == (h - 1)], na.rm = T)
     }
     plot.df <- data.frame(AC)
     g <- ggplot(data = plot.df) + geom_line(aes(x = 0:23, y = AC), group = "a", colour = "blue") + theme_bw() + labs(title = PID, y = "Activity Count", x = "Time of Day") + 
          geom_hline(yintercept = 100, colour = "red") + annotate("text", 2, 130, label = "Cutoff", size = 5)
     print(g)
     plot.df
}

construct.normal.features <- function(PID = PID, participant.1m = participant.1m) {
     result <- data.frame(PID = PID,
                          daily.activity.count.avg_cpm = NA,
                          daily.activity.count.sd_cpm = NA,
                          daily.steps.avg_spm = NA,
                          daily.steps.sd_spm = NA,
                          daily.a1.proportion.avg = NA,
                          daily.a1.proportion.sd = NA,
                          valid.days = max(participant.1m$valid.day))
     temp <- data.frame(matrix(nrow = 0, ncol = (ncol(result) - 2)))
     for(i in 1:max(participant.1m$valid.day)) {
          day.1m <- participant.1m[participant.1m$valid.day == i, ]
          activity.count.avg <- mean(day.1m$VM, na.rm = T)
          activity.count.sd <- sd(day.1m$VM, na.rm = T)
          steps.avg <- mean(day.1m$steps, na.rm = T)
          steps.sd <- sd(day.1m$steps, na.rm = T)
          a1.proportion.avg <- mean(day.1m$axis1 / day.1m$VM, na.rm = T)
          a1.proportion.sd <- sd(day.1m$axis1 / day.1m$VM, na.rm = T)
          temp <- rbind(temp, data.frame(daily.activity.count.avg_cpm = activity.count.avg,
                                         daily.activity.count.sd_cpm = activity.count.sd,
                                         daily.steps.avg = steps.avg,
                                         daily.steps.sd = steps.sd,
                                         daily.a1.proportion.avg = a1.proportion.avg,
                                         daily.a1.proportion.sd = a1.proportion.sd))
     }
     colnames(temp) <- colnames(result)[2:(ncol(result)-1)]
     result$daily.activity.count.avg_cpm = mean(temp$daily.activity.count.avg_cpm, na.rm = T)
     result$daily.activity.count.sd_cpm = mean(temp$daily.activity.count.sd_cpm, na.rm = T)
     result$daily.steps.avg_spm = mean(temp$daily.steps.avg_spm, na.rm = T)
     result$daily.steps.sd_spm = mean(temp$daily.steps.sd_spm, na.rm = T)
     result$daily.a1.proportion.avg = mean(temp$daily.a1.proportion.avg, na.rm = T)
     result$daily.a1.proportion.sd = mean(temp$daily.a1.proportion.sd, na.rm = T)
     result
}

race_character <- function(racevar = 4) {
     result <- "white"
     if(racevar == 1) {
          result <- "black"
     } else if(racevar == 2) {
          result <- "native american"
     } else if(racevar == 3) {
          result <- "asian"
     } else if(racevar == 5) {
          result <- "hawaiian"
     } else if(racevar == 6) {
          result <- "hispanic"
     } else if(racevar == 7) {
          result <- "other"
     }
     result
}

constructing.target.demographic.variables <- function(PID, target.df, mapping.df) {
     result <- data.frame(age = NA, bmi = NA, gender = NA, race = NA,
                          class.cvd = NA, class.mobilityImpaired = NA, class.cognitionImpaired = NA)
     maskid <- mapping.df$maskid[mapping.df$accpid == PID]
     result$age <- target.df$age[target.df$MaskID == maskid]
     result$bmi <- target.df$bmi[target.df$MaskID == maskid]
     result$gender <- target.df$gender_tscr[target.df$MaskID == maskid]
     result$race <- race_character(target.df$racevar[target.df$MaskID == maskid])
     result$class.cvd <- (target.df$sub_cvd[target.df$MaskID == maskid] == 1)
     walktime <- target.df$walk_time[target.df$MaskID == maskid]
     result$class.mobilityImpaired <- ((400 / walktime) < 0.8)
     result$class.cognitionImpaired <- (target.df$sub_3MSE[target.df$MaskID == maskid] == 1)
     result
}


construct.time.of.day.features <- function(PID = PID, participant.1m = participant.1m) {
     result <- data.frame(PID = PID,
                          morning.activity.count.avg_cpm = NA,
                          morning.activity.count.sd_cpm = NA,
                          morning.steps.avg_spm = NA,
                          morning.steps.sd_spm = NA,
                          morning.a1.proportion.avg = NA,
                          morning.a1.proportion.sd = NA,
                          noon.activity.count.avg_cpm = NA,
                          noon.activity.count.sd_cpm = NA,
                          noon.steps.avg_spm = NA,
                          noon.steps.sd_spm = NA,
                          noon.a1.proportion.avg = NA,
                          noon.a1.proportion.sd = NA,
                          afternoon.activity.count.avg_cpm = NA,
                          afternoon.activity.count.sd_cpm = NA,
                          afternoon.steps.avg_spm = NA,
                          afternoon.steps.sd_spm = NA,
                          afternoon.a1.proportion.avg = NA,
                          afternoon.a1.proportion.sd = NA,
                          evening.activity.count.avg_cpm = NA,
                          evening.activity.count.sd_cpm = NA,
                          evening.steps.avg_spm = NA,
                          evening.steps.sd_spm = NA,
                          evening.a1.proportion.avg = NA,
                          evening.a1.proportion.sd = NA,
                          valid.days = max(participant.1m$valid.day))
     temp.result <- data.frame(matrix(nrow = 0, ncol = (ncol(result) - 2)))
     for(i in 1:max(participant.1m$valid.day)) {
          temp <- data.frame(matrix(nrow = 1, ncol = (ncol(result) - 2)))
          colnames(temp) <- colnames(result)[2:(ncol(result)-1)]
          day.1m <- participant.1m[participant.1m$valid.day == i, ]
          # Morning: 8am to 11am
          temp.1m <- day.1m[which(day.1m$hour.of.day >= 8), ]
          temp.1m <- temp.1m[which(temp.1m$hour.of.day < 11), ]
          temp$morning.activity.count.avg_cpm <- mean(temp.1m$VM, na.rm = T)
          temp$morning.activity.count.sd_cpm <- sd(temp.1m$VM, na.rm = T)
          temp$morning.steps.avg_spm <- mean(temp.1m$steps, na.rm = T)
          temp$morning.steps.sd_spm <- sd(temp.1m$steps, na.rm = T)
          temp$morning.a1.proportion.avg <- mean(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          temp$morning.a1.proportion.sd <- sd(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          # Noon: 11am to 2pm
          temp.1m <- day.1m[which(day.1m$hour.of.day >= 11), ]
          temp.1m <- temp.1m[which(temp.1m$hour.of.day < 14), ]
          temp$noon.activity.count.avg_cpm <- mean(temp.1m$VM, na.rm = T)
          temp$noon.activity.count.sd_cpm <- sd(temp.1m$VM, na.rm = T)
          temp$noon.steps.avg_spm <- mean(temp.1m$steps, na.rm = T)
          temp$noon.steps.sd_spm <- sd(temp.1m$steps, na.rm = T)
          temp$noon.a1.proportion.avg <- mean(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          temp$noon.a1.proportion.sd <- sd(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          # Afternoon: 2pm to 5pm
          temp.1m <- day.1m[which(day.1m$hour.of.day >= 14), ]
          temp.1m <- temp.1m[which(temp.1m$hour.of.day < 17), ]
          temp$afternoon.activity.count.avg_cpm <- mean(temp.1m$VM, na.rm = T)
          temp$afternoon.activity.count.sd_cpm <- sd(temp.1m$VM, na.rm = T)
          temp$afternoon.steps.avg_spm <- mean(temp.1m$steps, na.rm = T)
          temp$afternoon.steps.sd_spm <- sd(temp.1m$steps, na.rm = T)
          temp$afternoon.a1.proportion.avg <- mean(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          temp$afternoon.a1.proportion.sd <- sd(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          # Evening: 5pm to 8pm
          temp.1m <- day.1m[which(day.1m$hour.of.day >= 17), ]
          temp.1m <- temp.1m[which(temp.1m$hour.of.day < 20), ]
          temp$evening.activity.count.avg_cpm <- mean(temp.1m$VM, na.rm = T)
          temp$evening.activity.count.sd_cpm <- sd(temp.1m$VM, na.rm = T)
          temp$evening.steps.avg_spm <- mean(temp.1m$steps, na.rm = T)
          temp$evening.steps.sd_spm <- sd(temp.1m$steps, na.rm = T)
          temp$evening.a1.proportion.avg <- mean(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          temp$evening.a1.proportion.sd <- sd(temp.1m$axis1 / temp.1m$VM, na.rm = T)
          
          temp.result <- rbind(temp.result, temp)
     }
     colnames(temp.result) <- colnames(result)[2:(ncol(result)-1)]
          
     result$morning.activity.count.avg_cpm = mean(temp.result$morning.activity.count.avg_cpm, na.rm = T)
     result$morning.activity.count.sd_cpm = mean(temp.result$morning.activity.count.sd_cpm, na.rm = T)
     result$morning.steps.avg_spm = mean(temp.result$morning.steps.avg_spm, na.rm = T)
     result$morning.steps.sd_spm = mean(temp.result$morning.steps.sd_spm, na.rm = T)
     result$morning.a1.proportion.avg = mean(temp.result$morning.a1.proportion.avg, na.rm = T)
     result$morning.a1.proportion.sd = mean(temp.result$morning.a1.proportion.sd, na.rm = T)
     
     result$noon.activity.count.avg_cpm = mean(temp.result$noon.activity.count.avg_cpm, na.rm = T)
     result$noon.activity.count.sd_cpm = mean(temp.result$noon.activity.count.sd_cpm, na.rm = T)
     result$noon.steps.avg_spm = mean(temp.result$noon.steps.avg_spm, na.rm = T)
     result$noon.steps.sd_spm = mean(temp.result$noon.steps.sd_spm, na.rm = T)
     result$noon.a1.proportion.avg = mean(temp.result$noon.a1.proportion.avg, na.rm = T)
     result$noon.a1.proportion.sd = mean(temp.result$noon.a1.proportion.sd, na.rm = T)
     
     result$afternoon.activity.count.avg_cpm = mean(temp.result$afternoon.activity.count.avg_cpm, na.rm = T)
     result$afternoon.activity.count.sd_cpm = mean(temp.result$afternoon.activity.count.sd_cpm, na.rm = T)
     result$afternoon.steps.avg_spm = mean(temp.result$afternoon.steps.avg_spm, na.rm = T)
     result$afternoon.steps.sd_spm = mean(temp.result$afternoon.steps.sd_spm, na.rm = T)
     result$afternoon.a1.proportion.avg = mean(temp.result$afternoon.a1.proportion.avg, na.rm = T)
     result$afternoon.a1.proportion.sd = mean(temp.result$afternoon.a1.proportion.sd, na.rm = T)
     
     result$evening.activity.count.avg_cpm = mean(temp.result$evening.activity.count.avg_cpm, na.rm = T)
     result$evening.activity.count.sd_cpm = mean(temp.result$evening.activity.count.sd_cpm, na.rm = T)
     result$evening.steps.avg_spm = mean(temp.result$evening.steps.avg_spm, na.rm = T)
     result$evening.steps.sd_spm = mean(temp.result$evening.steps.sd_spm, na.rm = T)
     result$evening.a1.proportion.avg = mean(temp.result$evening.a1.proportion.avg, na.rm = T)
     result$evening.a1.proportion.sd = mean(temp.result$evening.a1.proportion.sd, na.rm = T)
     result
}

construct.circadian.features <- function(PID, participant.1m, plot.it = F) {
     if(plot.it) {
          plotting.participant.daily.activity(participant.1m, PID)
     }
     
     ac <- rep(0, 24)
     for(i in 0:23) {
          temp.ac <- mean(participant.1m$VM[participant.1m$hour.of.day == i], na.rm = T)
          if(!is.na(temp.ac)) {
               ac[i + 1] <- temp.ac
          }
     }
     day.start <- min(which(ac[5:24] > 100)) + 4
     day.end <- max(which(ac > 100))
     peak.ac <- max(ac)
     peak.hour <- which(ac == peak.ac)
     nadir.ac <- min(ac[ac > 100])
     nadir.hour <- which(ac == nadir.ac)
     diff.peakNadir.ac <- peak.ac - nadir.ac
     diff.peakNadir.hour <- abs(peak.hour - nadir.hour)
     mesor.ac <- median(ac[ac > 100])
     temp <- abs(mesor.ac - ac)
     mesor.hour <- which(temp == min(temp))
     result <- data.frame(PID = PID,
                          peak.ac_cpm = peak.ac,
                          peak.hour = peak.hour,
                          nadir.ac_cpm = nadir.ac,
                          nadir.hour = nadir.hour,
                          diff.peakNadir.ac_cpm = diff.peakNadir.ac,
                          diff.peakNadir.hour = diff.peakNadir.hour,
                          median.ac_cpm = mesor.ac,
                          median.hour = mesor.hour,
                          day.start_hour = day.start,
                          day.end_hour = day.end,
                          valid.days = max(participant.1m$valid.day))
     result
}

# Removing non wears completely -------------------------------------------------------

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - 03_28_16/LIFE accelerometer data - minute data - 03_28_16/"
l <- list.files(path = minute_directory)


for(i in 1:length(l)) {
     
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
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - wear time only - 04_06_16/"
l <- list.files(path = minute_directory)
daily.df <- data.frame(matrix(nrow = 0, ncol = 8))
for(i in 1:length(l)) {
     
     fileName <- l[i]
     PID <- substr(fileName, 1, 7)

     print(paste(i, " out of 1212 - (", PID, ")", sep = ""))
     load(paste(minute_directory, fileName, sep = ""))
     
     ppt.df <- construct.normal.features(PID, participant.1m)
     daily.df <- rbind(daily.df, ppt.df)
     colnames(daily.df) <- colnames(ppt.df)
}
daily.df$PID <- as.character(daily.df$PID)
save(daily.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")
rm(list = ls())



# Appending target and demographic variables to the dataset

target.df <- read_dta("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d02_target variables - raw/Baseline_Key_Variables_v2_2.dta")
mapping.df <- read_dta("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d02_target variables - raw/ACCPID to LIFE maskid.dta")
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")

complete.df <- data.frame(matrix(nrow = 0, ncol = (ncol(daily.df) + 7)))
for(i in 1:nrow(daily.df)) {
     target_n_demographics <- constructing.target.demographic.variables(daily.df$PID[i], target.df, mapping.df)
     complete.df <- rbind(complete.df, cbind(daily.df[i, ], target_n_demographics))
     colnames(complete.df) <- c(colnames(daily.df), colnames(target_n_demographics))
}
daily.df <- complete.df
save(daily.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")
rm(list = ls())




# Time.of.day dataset --------------------------------------------------------
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - wear time only - 04_06_16/"
l <- list.files(path = minute_directory)
time.of.day.df <- data.frame(matrix(nrow = 0, ncol = 26))
for(i in 1:length(l)) {
     
     fileName <- l[i]
     PID <- substr(fileName, 1, 7)
     
     print(paste(i, " out of 1212 - (", PID, ")", sep = ""))
     load(paste(minute_directory, fileName, sep = ""))
     
     ppt.df <- construct.time.of.day.features(PID, participant.1m)
     time.of.day.df <- rbind(time.of.day.df, ppt.df)
     colnames(time.of.day.df) <- colnames(ppt.df)
}
time.of.day.df$PID <- as.character(time.of.day.df$PID)


target.df <- read_dta("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d02_target variables - raw/Baseline_Key_Variables_v2_2.dta")
mapping.df <- read_dta("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d02_target variables - raw/ACCPID to LIFE maskid.dta")
complete.df <- data.frame(matrix(nrow = 0, ncol = (ncol(time.of.day.df) + 7)))
for(i in 1:nrow(time.of.day.df)) {
     target_n_demographics <- constructing.target.demographic.variables(time.of.day.df$PID[i], target.df, mapping.df)
     complete.df <- rbind(complete.df, cbind(time.of.day.df[i, ], target_n_demographics))
     colnames(complete.df) <- c(colnames(time.of.day.df), colnames(target_n_demographics))
}
time.of.day.df <- complete.df
save(time.of.day.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d02_timeOfDay_second.Rdata")


rm(list = ls())



# Circadian approach ----------------------------------------------------------
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - wear time only - 04_06_16/"
l <- list.files(path = minute_directory)

circadian.df <- data.frame(matrix(nrow = 0, ncol = 12))
for(i in 1:length(l)) {
     
     fileName <- l[i]
     PID <- substr(fileName, 1, 7)
     
     print(paste(i, " out of 1212 - (", PID, ")", sep = ""))
     load(paste(minute_directory, fileName, sep = ""))
     
     ppt.df <- construct.circadian.features(PID, participant.1m, plot.it = F)
     circadian.df <- rbind(circadian.df, ppt.df[1, ])
     colnames(circadian.df) <- colnames(ppt.df)
}
circadian.df$PID <- as.character(circadian.df$PID)
save(circadian.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d03_circadian_third.Rdata")


load(file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")

complete.df <- data.frame(matrix(nrow = 0, ncol = (ncol(daily.df) + ncol(circadian.df) - 2)))
for(i in 1:nrow(circadian.df)) {
     PID <- circadian.df$PID[i]
     temp.df <- cbind(daily.df[daily.df$PID == PID, c(1:7)], circadian.df[i, c(2:ncol(circadian.df))], daily.df[daily.df$PID == PID, c(9:ncol(daily.df))])
     complete.df <- rbind(complete.df, temp.df)
}
circadian.df <- complete.df
save(circadian.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d03_circadian_third.Rdata")
rm(list = ls())

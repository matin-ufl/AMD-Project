library(reshape2)
library(ggplot2)

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Matin - Exploration/")


# Target Variables and Log Activity Count -------------------------

minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer hour data - wear time only - 04_06_16/"
l <- dir(minute_directory)
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")
hourly.ac.df <- data.frame(matrix(nrow = length(l), ncol = 28))
colnames(hourly.ac.df) <- c("PID", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00",
                            "06:00", "07:00", "08:00", "09:00", "10:00", "11:00",
                            "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "mobility.impaired", "cognition.dysfunction", "CVD")
for(i in 1:length(l)) {
     hourly.ac.df$PID[i] <- as.character(substr(l[i], 1, 7))
     hourly.ac.df$mobility.impaired[i] <- as.character(daily.df$class.mobilityImpaired[daily.df$PID == hourly.ac.df$PID[i]])
     hourly.ac.df$cognition.dysfunction[i] <- as.character(daily.df$class.cognitionImpaired[daily.df$PID == hourly.ac.df$PID[i]])
     hourly.ac.df$CVD[i] <- as.character(daily.df$class.cvd[daily.df$PID == hourly.ac.df$PID[i]])
     load(paste(minute_directory, l[i], sep = ""))
     hourly.ac.df[i, 2:25] <- participant.1h$log.activity.count_hourly
}

rm(i, l, minute_directory, participant.1h)
hourly.ac.df[is.na(hourly.ac.df)] <- 0
hourly.ac.df[hourly.ac.df < 0] <- 0
hourly.ac.df$mobility.impaired <- as.factor(hourly.ac.df$mobility.impaired)
hourly.ac.df$cognition.dysfunction <- as.factor(hourly.ac.df$cognition.dysfunction)
hourly.ac.df$CVD <- as.factor(hourly.ac.df$CVD)
hourly.logAC.df <- hourly.ac.df
save(hourly.logAC.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/01_all_logAC.RData")
rm(list = ls())


# Plotting all the data for Mobility ==========================
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/01_all_logAC.RData")

# Not very meaningful
# plot.df <- cbind(melt(hourly.logAC.mobility.df[, 1:25]), rep(hourly.logAC.mobility.df$mobility.impaired, 24))
# colnames(plot.df) <- c("PID", "Hour", "Log.Activity.Count", "Mobility.Impaired")
# plot.df$Mobility.Impaired <- factor(plot.df$Mobility.Impaired)
# g <- ggplot(data = plot.df) + geom_line(aes(x = Hour, y = Log.Activity.Count, group = PID, colour = Mobility.Impaired))
# g + scale_colour_manual(values = c("red", "blue")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting groups of Mobility =================================
plot.df <- data.frame(matrix(nrow = 2, ncol = (ncol(hourly.logAC.df)-3)))
colnames(plot.df) <- colnames(hourly.logAC.df)[2:(ncol(hourly.logAC.df)-2)]
for(i in 1:(ncol(plot.df) - 1)) {
     plot.df[1, i] <- mean(hourly.logAC.df[hourly.logAC.df$mobility.impaired == F, i+1], na.rm = T)
     plot.df[2, i] <- mean(hourly.logAC.df[hourly.logAC.df$mobility.impaired == T, i+1], na.rm = T)
}
plot.df$mobility.impaired[1] <- F
plot.df$mobility.impaired[2] <- T

a <- melt(plot.df)
g <- ggplot(data = a) + geom_line(aes(x = variable, y = value, group = mobility.impaired, colour = mobility.impaired), size = 1.5)
g + scale_colour_manual(values = c("red", "blue"), name = "Mobility Impaired (<0.8 m/s)") +
     labs(x = "hour of day", y = "Log Activity Count", title = "Difference between different mobility groups") +
     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plotting groups of CVD =================================
plot.df <- data.frame(matrix(nrow = 2, ncol = (ncol(hourly.logAC.df)-3)))
colnames(plot.df) <- colnames(hourly.logAC.df)[c(2:25, 28)]
for(i in 1:(ncol(plot.df) - 1)) {
     plot.df[1, i] <- mean(hourly.logAC.df[hourly.logAC.df$CVD == F, i+1], na.rm = T)
     plot.df[2, i] <- mean(hourly.logAC.df[hourly.logAC.df$CVD == T, i+1], na.rm = T)
}
plot.df$CVD[1] <- F
plot.df$CVD[2] <- T

a <- melt(plot.df)
g <- ggplot(data = a) + geom_line(aes(x = variable, y = value, group = CVD, colour = CVD), size = 1.5)
g + scale_colour_manual(values = c("red", "blue"), name = "CVD") +
     labs(x = "hour of day", y = "Log Activity Count") +
     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting groups of Cognition =================================
plot.df <- data.frame(matrix(nrow = 2, ncol = (ncol(hourly.logAC.df)-3)))
colnames(plot.df) <- colnames(hourly.logAC.df)[c(2:25, 27)]
for(i in 1:(ncol(plot.df) - 1)) {
     plot.df[1, i] <- mean(hourly.logAC.df[hourly.logAC.df$cognition.dysfunction == F, i+1], na.rm = T)
     plot.df[2, i] <- mean(hourly.logAC.df[hourly.logAC.df$cognition.dysfunction == T, i+1], na.rm = T)
}
plot.df$cognition.dysfunction[1] <- F
plot.df$cognition.dysfunction[2] <- T

a <- melt(plot.df)
g <- ggplot(data = a) + geom_line(aes(x = variable, y = value, group = cognition.dysfunction, colour = cognition.dysfunction), size = 1.5)
g + scale_colour_manual(values = c("red", "blue"), name = "Cognition Dysfunction") +
     labs(x = "hour of day", y = "Log Activity Count") +
     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(list = ls())


# Accumulative ---------------------------------------------------
minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer hour data - wear time only - 04_06_16/"
l <- dir(minute_directory)
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")
hourly.ac.df <- data.frame(matrix(nrow = length(l), ncol = 28))
colnames(hourly.ac.df) <- c("PID", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00",
                            "06:00", "07:00", "08:00", "09:00", "10:00", "11:00",
                            "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                            "18:00", "19:00", "20:00", "21:00", "22:00", "23:00",
                            "mobility.impaired", "cognition.dysfunction", "CVD")
for(i in 1:length(l)) {
     hourly.ac.df$PID[i] <- as.character(substr(l[i], 1, 7))
     hourly.ac.df$mobility.impaired[i] <- as.character(daily.df$class.mobilityImpaired[daily.df$PID == hourly.ac.df$PID[i]])
     hourly.ac.df$cognition.dysfunction[i] <- as.character(daily.df$class.cognitionImpaired[daily.df$PID == hourly.ac.df$PID[i]])
     hourly.ac.df$CVD[i] <- as.character(daily.df$class.cvd[daily.df$PID == hourly.ac.df$PID[i]])
     load(paste(minute_directory, l[i], sep = ""))
     hourly.ac.df[i, 2:25] <- participant.1h$activity.count_hourly
}

rm(i, l, minute_directory, participant.1h)
hourly.ac.df[hourly.ac.df < 0] <- 0
hourly.ac.df$mobility.impaired <- as.factor(hourly.ac.df$mobility.impaired)
hourly.ac.df$cognition.dysfunction <- as.factor(hourly.ac.df$cognition.dysfunction)
hourly.ac.df$CVD <- as.factor(hourly.ac.df$CVD)
hourly.cummulative.df <- hourly.ac.df
for(i in 1:nrow(hourly.cummulative.df)) {
     for(j in 2:25) {
          hourly.cummulative.df[i, j] <- sum(hourly.ac.df[i, 2:j])
     }
}
rm(i, j)
save(hourly.cummulative.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/02_all_cummulativeAC.RData")
rm(list = ls())



# Cummulative plots =================================
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/02_all_cummulativeAC.RData")

# Mobility ######################
plot.df <- data.frame(matrix(nrow = 2, ncol = (ncol(hourly.cummulative.df)-3)))
colnames(plot.df) <- colnames(hourly.cummulative.df)[2:(ncol(hourly.cummulative.df)-2)]
for(i in 1:(ncol(plot.df) - 1)) {
     plot.df[1, i] <- mean(hourly.cummulative.df[hourly.cummulative.df$mobility.impaired == F, i+1], na.rm = T)
     plot.df[2, i] <- mean(hourly.cummulative.df[hourly.cummulative.df$mobility.impaired == T, i+1], na.rm = T)
}
plot.df$mobility.impaired[1] <- F
plot.df$mobility.impaired[2] <- T

a <- melt(plot.df)
g <- ggplot(data = a) + geom_line(aes(x = variable, y = value, group = mobility.impaired, colour = mobility.impaired), size = 1.5)
g + scale_colour_manual(values = c("red", "blue"), name = "Mobility Impaired (<0.8 m/s)") +
     labs(x = "hour of day", y = "Activity Count", title = "Difference between different mobility groups") +
     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# CVD ######################
plot.df <- data.frame(matrix(nrow = 2, ncol = (ncol(hourly.cummulative.df)-3)))
colnames(plot.df) <- colnames(hourly.cummulative.df)[c(2:25, 28)]
for(i in 1:(ncol(plot.df) - 1)) {
     plot.df[1, i] <- mean(hourly.cummulative.df[hourly.cummulative.df$CVD == F, i+1], na.rm = T)
     plot.df[2, i] <- mean(hourly.cummulative.df[hourly.cummulative.df$CVD == T, i+1], na.rm = T)
}
plot.df$CVD[1] <- F
plot.df$CVD[2] <- T

a <- melt(plot.df)
g <- ggplot(data = a) + geom_line(aes(x = variable, y = value, group = CVD, colour = CVD), size = 1.5)
g + scale_colour_manual(values = c("red", "blue"), name = "CVD") +
     labs(x = "hour of day", y = "Activity Count") +
     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cognition ##################
plot.df <- data.frame(matrix(nrow = 2, ncol = (ncol(hourly.cummulative.df)-3)))
colnames(plot.df) <- colnames(hourly.cummulative.df)[c(2:25, 27)]
for(i in 1:(ncol(plot.df) - 1)) {
     plot.df[1, i] <- mean(hourly.cummulative.df[hourly.cummulative.df$cognition.dysfunction == F, i+1], na.rm = T)
     plot.df[2, i] <- mean(hourly.cummulative.df[hourly.cummulative.df$cognition.dysfunction == T, i+1], na.rm = T)
}
plot.df$cognition.dysfunction[1] <- F
plot.df$cognition.dysfunction[2] <- T

a <- melt(plot.df)
g <- ggplot(data = a) + geom_line(aes(x = variable, y = value, group = cognition.dysfunction, colour = cognition.dysfunction), size = 1.5)
g + scale_colour_manual(values = c("red", "blue"), name = "Cognition Dysfunction") +
     labs(x = "hour of day", y = "Activity Count") +
     theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(list = ls())







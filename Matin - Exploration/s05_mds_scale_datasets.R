library(ggplot2)

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Matin - Exploration/")

# Daily dataset ---------------------------------------------------
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")
accelerometer.df <- daily.df[, c(1:7, 13:15)]
scaled.daily.df <- accelerometer.df

for(i in 2:(ncol(scaled.daily.df) - 3)) {
     scaled.daily.df[, i] <- (scaled.daily.df[, i] - min(scaled.daily.df[, i])) / (max(scaled.daily.df[, i]) - min(scaled.daily.df[, i]))
}
rm(i)
d <- daisy(scaled.daily.df[, -c(1, 8:10)])
fit <- cmdscale(d, eig = T, k = 2)
mds2d.daily.df <- data.frame(PID = scaled.daily.df$PID, Coordinate.1 = fit$points[, 1], Coordinate.2 = fit$points[, 2],
                             class.mobilityImpaired = scaled.daily.df$class.mobilityImpaired,
                             class.cognitionImpaired = scaled.daily.df$class.cognitionImpaired,
                             class.cvd = scaled.daily.df$class.cvd)

rm(fit, d, accelerometer.df)
save.image(file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d04_daily_everything.Rdata")

g <- ggplot(data = mds2d.daily.df) + geom_point(aes(x = Coordinate.1, y = Coordinate.2, colour = class.cognitionImpaired), size = 5)
g + scale_colour_manual(values = c("red", "blue")) + theme_classic()

rm(list = ls())


# Time of Day dataset ---------------------------------------------------
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d02_timeOfDay_second.Rdata")
for(j in 2:(ncol(time.of.day.df) - 3)) {
     time.of.day.df[which(is.na(time.of.day.df[, j])), j] <- 0
}
accelerometer.df <- time.of.day.df[, c(1:25, 31:33)]
scaled.timeOfDay.df <- accelerometer.df

for(i in 2:(ncol(scaled.timeOfDay.df) - 3)) {
     scaled.timeOfDay.df[, i] <- (scaled.timeOfDay.df[, i] - min(scaled.timeOfDay.df[, i])) / (max(scaled.timeOfDay.df[, i]) - min(scaled.timeOfDay.df[, i]))
}
rm(i, j)
d <- daisy(scaled.timeOfDay.df[, -c(1, 26:28)])
fit <- cmdscale(d, eig = T, k = 2)
mds2d.timeOfDay.df <- data.frame(PID = scaled.timeOfDay.df$PID, Coordinate.1 = fit$points[, 1], Coordinate.2 = fit$points[, 2],
                             class.mobilityImpaired = scaled.timeOfDay.df$class.mobilityImpaired,
                             class.cognitionImpaired = scaled.timeOfDay.df$class.cognitionImpaired,
                             class.cvd = scaled.timeOfDay.df$class.cvd)

rm(fit, d, accelerometer.df)
save.image(file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d05_timeOfDay_everything.Rdata")

g <- ggplot(data = mds2d.timeOfDay.df) + geom_point(aes(x = Coordinate.1, y = Coordinate.2, colour = class.mobilityImpaired), size = 5)
g + scale_colour_manual(values = c("red", "blue")) + theme_classic()

rm(list = ls())

# Circadian dataset ---------------------------------------------------
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d03_circadian_third.Rdata")
accelerometer.df <- circadian.df[, c(1:17, 23:25)]
scaled.circadian.df <- accelerometer.df

for(i in 2:(ncol(scaled.circadian.df) - 3)) {
     scaled.circadian.df[, i] <- (scaled.circadian.df[, i] - min(scaled.circadian.df[, i])) / (max(scaled.circadian.df[, i]) - min(scaled.circadian.df[, i]))
}
rm(i)
d <- daisy(scaled.circadian.df[, -c(1, 18:20)])
fit <- cmdscale(d, eig = T, k = 2)
mds2d.circadian.df <- data.frame(PID = scaled.circadian.df$PID, Coordinate.1 = fit$points[, 1], Coordinate.2 = fit$points[, 2],
                                 class.mobilityImpaired = scaled.circadian.df$class.mobilityImpaired,
                                 class.cognitionImpaired = scaled.circadian.df$class.cognitionImpaired,
                                 class.cvd = scaled.circadian.df$class.cvd)

rm(fit, d, accelerometer.df)
save.image(file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d06_circadian_everything.Rdata")

g <- ggplot(data = mds2d.circadian.df) + geom_point(aes(x = Coordinate.1, y = Coordinate.2, colour = class.mobilityImpaired), size = 5)
g + scale_colour_manual(values = c("red", "blue")) + theme_classic()

rm(list = ls())




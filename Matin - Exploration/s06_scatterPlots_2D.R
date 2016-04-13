library(ggplot2)
library(gridExtra)

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Matin - Exploration/")

# Loading daily dataset
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d04_daily_everything.Rdata")
# Loading time of day dataset
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d05_timeOfDay_everything.Rdata")
# Loading circadian dataset
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d06_circadian_everything.Rdata")

rm(circadian.df, daily.df, scaled.daily.df, scaled.circadian.df, scaled.timeOfDay.df, time.of.day.df)


# Daily scatter plot --------------------------
target <- cbind(t(mds2d.daily.df$class.mobilityImpaired), t(mds2d.timeOfDay.df$class.mobilityImpaired), t(mds2d.circadian.df$class.mobilityImpaired),
                t(mds2d.daily.df$class.cognitionImpaired), t(mds2d.timeOfDay.df$class.cognitionImpaired), t(mds2d.circadian.df$class.cognitionImpaired),
                t(mds2d.daily.df$class.cvd), t(mds2d.timeOfDay.df$class.cvd), t(mds2d.circadian.df$class.cvd))
target[target == 1] <- "FALSE"
target[target == 2] <- "TRUE"
class.variable <- cbind(t(rep("mobility impairment", 3636)), t(rep("cognition dysfunction", 3636)), t(rep("CVD", 3636)))
dataset <- cbind(t(rep("Daily", 1212)), t(rep("Times of Day", 1212)), t(rep("Circadian", 1212)))
dataset <- rep(dataset, 3)
dataset <- factor(dataset, levels = c("Daily", "Times of Day", "Circadian"))

Coordinate.1 <- cbind(t(mds2d.daily.df$Coordinate.1), t(mds2d.timeOfDay.df$Coordinate.1), t(mds2d.circadian.df$Coordinate.1))
Coordinate.1 <- rep(Coordinate.1, 3)
Coordinate.2 <- cbind(t(mds2d.daily.df$Coordinate.2), t(mds2d.timeOfDay.df$Coordinate.2), t(mds2d.circadian.df$Coordinate.2))
Coordinate.2 <- rep(Coordinate.2, 3)

plot.df <- data.frame(Coordinate.1, Coordinate.2, target = as.factor(t(target)), class.variable = t(class.variable), dataset = dataset)
g <- ggplot(data = plot.df) + geom_point(aes(x = Coordinate.1, y = Coordinate.2, colour = target), size = 3)
g + scale_colour_manual(values = c("black", "red")) + facet_grid(class.variable~dataset) + theme_bw()

rm(list = ls())
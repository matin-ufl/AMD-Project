library(ggplot2)
library(reshape2)

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Matin - Exploration/")

# Loading and correcting outputs --------------------------
mobility.results <- read.csv("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/out01_classification_mobility.csv")
cognition.results <- read.csv("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/out02_classification_cognition.csv")
cvd.results <- read.csv("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d04_exploration datasets/out03_classification_cvd.csv")

mobility.results$dataset <- factor(x = mobility.results$dataset, levels = levels(mobility.results$dataset)[c(2, 3, 1)])
mobility.results$method <- factor(x = mobility.results$method, levels = mobility.results$method[1:8])
cognition.results$dataset <- factor(x = cognition.results$dataset, levels = levels(cognition.results$dataset)[c(2, 3, 1)])
cognition.results$method <- factor(x = cognition.results$method, levels = cognition.results$method[1:8])
cvd.results$dataset <- factor(x = cvd.results$dataset, levels = levels(cvd.results$dataset)[c(2, 3, 1)])
cvd.results$method <- factor(x = cvd.results$method, levels = cvd.results$method[1:8])

# Plottings -----------------------------
# Mobility
g.accuracy <- ggplot(data = mobility.results[, 1:3]) + geom_bar(aes(x = method, y = accuracy, fill = method), stat = "identity", colour = "black")
g.accuracy <- g.accuracy + facet_grid(.~dataset) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Mobility Impairment", x = "")

g.sensitivity <- ggplot(data = mobility.results[, c(1:2, 4)]) + geom_bar(aes(x = method, y = sensitivity, fill = method), stat = "identity", colour = "black")
g.sensitivity <- g.sensitivity + facet_grid(.~dataset) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(g.accuracy, g.sensitivity, ncol = 1)

# CVD
g.accuracy <- ggplot(data = cvd.results[, 1:3]) + geom_bar(aes(x = method, y = accuracy, fill = method), stat = "identity", colour = "black")
g.accuracy <- g.accuracy + facet_grid(.~dataset) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Cardio Vascular Disease (CVD)", x = "")

g.sensitivity <- ggplot(data = cvd.results[, c(1:2, 4)]) + geom_bar(aes(x = method, y = sensitivity, fill = method), stat = "identity", colour = "black")
g.sensitivity <- g.sensitivity + facet_grid(.~dataset) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(g.accuracy, g.sensitivity, ncol = 1)


# Cognition
g.accuracy <- ggplot(data = cognition.results[, 1:3]) + geom_bar(aes(x = method, y = accuracy, fill = method), stat = "identity", colour = "black")
g.accuracy <- g.accuracy + facet_grid(.~dataset) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Cognition Dysfunction", x = "")

g.sensitivity <- ggplot(data = cognition.results[, c(1:2, 4)]) + geom_bar(aes(x = method, y = sensitivity, fill = method), stat = "identity", colour = "black")
g.sensitivity <- g.sensitivity + facet_grid(.~dataset) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(g.accuracy, g.sensitivity, ncol = 1)



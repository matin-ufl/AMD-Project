library(stats)

# Functions -----------------------------------------------
clr <- function() {cat(rep("\n", 50))}

cosine.fit <- function(VM.ac.w) {
     ssp <- spectrum(VM.ac.w) # find the spectrums of the signal
     per <- 1/ssp$freq[ssp$spec == max(ssp$spec)] # find the frequency where the peak happens
     t <- 1:length(VM.ac.w)
     reslm <- lm(VM.ac.w ~ sin(2*pi/per*t) + cos(2*pi/per*t))
     result <- list(frequency = per, fitted.lm = reslm)
     result
}

# Script --------------------------------------------------
setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")

minute_directory <- "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d01_accelerometer minute data - nonwear replaced by negative - 04_11_16/"
l <- list.files(path = minute_directory)
circadian.df <- data.frame(matrix(nrow = 0, ncol = 11))
counter <- 0
for(fileName in l) {
     PID <- substr(fileName, start = 1, stop = 7)
     load(paste(minute_directory, fileName, sep = ""))
     counter <- counter + 1
     clr()
     print(paste(counter, " out of ", length(l), " (", PID, ")", sep = ""))
     a <- which(participant.1m$VM > -1)
     b <- diff(a)
     c <- which(b > 1)
     start.idx <- min(a)
     end.idx <- NULL
     for(cc in c) {
          start.idx <- c(start.idx, a[cc + 1])
          end.idx <- c(end.idx, a[cc])
     }
     end.idx <- c(end.idx, max(a))
     rm(a, b, c, cc)
     circadian.variables <- data.frame(matrix(nrow = 0, ncol = 5))
     for(i in 1:length(start.idx)) {
          VM.log10 <- participant.1m$VM[start.idx[i]:end.idx[i]]
          fitResult <- cosine.fit(VM.log10)
          
          temp <- data.frame(freq = fitResult$frequency,
                             mesor = fitResult$fitted.lm$coefficients[[1]], amp = abs(fitResult$fitted.lm$coefficients[[3]]),
                             fstat = summary(fitResult$fitted.lm)$fstatistic[[1]], rsquared = summary(fitResult$fitted.lm)$adj.r.squared)
          circadian.variables <- rbind(circadian.variables, temp)
          colnames(circadian.variables) <- colnames(temp)
     }
     rm(temp, i, VM.log10, fitResult)
     temp <- data.frame(PID = PID,
                        amplitude.avg_ac = mean(circadian.variables$amp, na.rm = T), amplitude.sd_ac = sd(circadian.variables$amp, na.rm = T),
                        freq.avg_min = mean(circadian.variables$freq, na.rm = T), freq.sd_min = sd(circadian.variables$freq, na.rm = T),
                        mesor.avg_ac = mean(circadian.variables$mesor, na.rm = T), mesor.sd_ac = sd(circadian.variables$mesor, na.rm = T),
                        fstat.avg = mean(circadian.variables$fstat, na.rm = T), fstat.sd = sd(circadian.variables$fstat, na.rm = T),
                        rsquared.avg = mean(circadian.variables$rsquared, na.rm = T), rsquared.sd = sd(circadian.variables$rsquared, na.rm = T))
     circadian.df <- rbind(circadian.df, temp)
     colnames(circadian.df) <- colnames(temp)
     rm(temp, circadian.variables, start.idx, end.idx)
}
rm(fileName, counter, participant.1m, PID)

# Appneding with the normal features
load("~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d01_daily_baseline.Rdata")
newCircadian.df <- cbind(daily.df[, c(1:7)], circadian.df[, c(2:ncol(circadian.df))], daily.df[, c(8:ncol(daily.df))])

save(newCircadian.df, file = "~/Dropbox/Courses/2016/Spring/Analysis of Multivariate Data/Project/Datasets/d03_datasets for analysis/d07_newCircadian.Rdata")

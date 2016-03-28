setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")
library(PhysicalActivity)
source("f01_functions.R")

# Temporary for easier file selection
setwd("~/../../Volumes/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/")

# Select PID_VC_HID
clr()
load("PID_VC_HID.Rdata")

valid.files <- valid_participants(PID_VC_HID = REF, valid.days = 5)
rm(REF)

# Creating new files without outlier points
for (i in 1:nrow(valid.files)) {
     PID <- valid.files$pid[i]
     HID <- paste("HID", valid.files$HID[i], ".Rdata", sep = "")
     print(paste(i, " out of ", nrow(valid.files), " - Being processed... ", HID, " PID (", PID, ")", sep = ""))
     load(HID)
     AC.1m <- shrink.to.1m(AC.1s, PID)
     save(AC.1m, file = paste("../LIFE accelerometer data - minute data - 03_28_16/", PID, ".Rdata"))
}

# Finalizing
rm(AC.1s, AC.1m, PID, HID, i, valid.files)

setwd("~/Workspaces/R workspace/Analysis of Multivariate Data/AMD-Project/Dataset Creation/")

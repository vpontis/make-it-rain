setwd('~/Dropbox/Classes/6.UAP/')
train_small <- read.csv("~/Dropbox (Personal)/Classes/6.UAP/data/train_small.csv", na.strings="nan")

# map TimeToEnd  to a vector of numbers
train_small$TimeToEnd <- as.vector(sapply(strsplit(as.character(train_small$TimeToEnd), " "), as.double))
train_small$DistanceToRadar <- as.vector(sapply(strsplit(as.character(train_small$DistanceToRadar), " "), as.double))
train_small$RR1 <- as.vector(sapply(strsplit(as.character(train_small$RR1), " "), as.double))

# note the number of measurements each hour
train_small$Measurements <- sapply(train_small$TimeToEnd, length)

# imports sigmoid
library(pracma)

# for each row look at RR1 and TimeToEnd
# RR1 -- take the mean
# TimeToEnd -- (max - min + 6) / 60
# Prediction -- sigmoid(RR1 * TimeToEnd)
#   Modify prediction w/ displacements for different values

# Remove values of RR1 that signify an incorrect measurement
# make sure that these are all of the incorrect values
incorrect_vals <- c(-99000, -99900, -99001, -99003, 999.0)

get_adjusted_time <- function(times_to_end) {
  avg <- (max(times_to_end) - min(times_to_end) + 6)/60
  return(avg)
}

get_adjusted_mean <- function(rr1_vals) {
  rr1_vals <- rr1_vals[!is.element(rr1_vals, incorrect_vals)]
  return(mean(rr1_vals))
}

train_small$AdjustedTime <- sapply(train_small$TimeToEnd, get_adjusted_time)
train_small$AvgRR1 <- sapply(train_small$RR1, get_adjusted_mean)

combine_rr1_and_times <- function(rr1, time) {
  return(sigmoid(rr1, time))
}

train_small$Prediction0 <- mapply(combine_rr1_and_times, train_small$AvgRR1, train_small$AdjustedTime)

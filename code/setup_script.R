# imports sigmoid
library(pracma)
library(fBasics)
library(data.table)

setwd('~/Dropbox/Classes/6.UAP/')
trainSmall <- fread("./data/train_small.csv", na.strings="nan", stringsAsFactors=F, showProgress=T)
# trainSmall <- fread("./data/test_2014.csv", na.strings="nan")

# map TimeToEnd  to a vector of numbers
# trainSmall$TimeToEnd <- as.vector(sapply(strsplit(as.character(trainSmall$TimeToEnd), " "), as.double))
# trainSmall$DistanceToRadar <- as.vector(sapply(strsplit(as.character(trainSmall$DistanceToRadar), " "), as.double))
# trainSmall$RR1 <- as.vector(sapply(strsplit(as.character(trainSmall$RR1), " "), as.double))


# Remove values of RR1 that signify an incorrect measurement
# make sure that these are all of the incorrect values
incorrectVals <- c(-99000, -99900, -99001, -99003, 999.0)

GetAdjustedMean <- function(vals) {
  vals <- vals[!is.element(vals, incorrectVals)]
  calculatedMean <- mean(vals)
  if (is.nan(calculatedMean)) {
    return(0)
  } else {
    return(calculatedMean)
  }
}

ProcessColumn <- function(dataframe, columnName) {
  # convert the factor to a string
  # split the string on spaces
  # convert each element into a floating point number
  # convert this to a vector
  dataframe[[columnName]] <- as.vector(sapply(strsplit(as.character(dataframe[[columnName]]), " "), as.double))
  avgColumnName <- paste('Avg', columnName, sep='')
  dataframe[[avgColumnName]] <- sapply(dataframe[[columnName]], GetAdjustedMean)
  return(dataframe)
}

MassageData <- function(dataframe) {
  doNotProcess <- c('Id', 'Expected')
  columnNames <- names(dataframe)
  columnNames <- columnNames[!is.element(columnNames, doNotProcess)]

  for (column in columnNames) {
    dataframe <- ProcessColumn(dataframe, column)
  }

  # note the number of measurements each hour
  dataframe$NumMeasurements <- sapply(dataframe$TimeToEnd, length)
  return(dataframe)
}

trainSmall <- MassageData(trainSmall)

# for each row look at RR1 and TimeToEnd
# RR1 -- take the mean
# TimeToEnd -- (max - min + 6) / 60
# Prediction -- sigmoid(RR1 * TimeToEnd)
#   Modify prediction w/ displacements for different values


GetAdjustedTime <- function(timesToEnd) {
  avg <- (max(timesToEnd) - min(timesToEnd) + 6)/60
  return(avg)
}

trainSmall$AdjustedTime <- sapply(trainSmall$TimeToEnd, GetAdjustedTime)
trainSmall$AvgRR1 <- sapply(trainSmall$RR1, GetAdjustedMean)

CombineRr1AndTimes <- function(rr1, time) {
  vals <- rr1 * time + 0:69
  return(sigmoid(vals))
}

trainSmall$Prediction <- t(mapply(CombineRr1AndTimes, trainSmall$AvgRR1, trainSmall$AdjustedTime))


write_solution_to_csv <- function(prediction_matrix, output_file) {
  solution_header <- "Id,Predicted0,Predicted1,Predicted2,Predicted3,Predicted4,Predicted5,Predicted6,Predicted7,Predicted8,Predicted9,Predicted10,Predicted11,Predicted12,Predicted13,Predicted14,Predicted15,Predicted16,Predicted17,Predicted18,Predicted19,Predicted20,Predicted21,Predicted22,Predicted23,Predicted24,Predicted25,Predicted26,Predicted27,Predicted28,Predicted29,Predicted30,Predicted31,Predicted32,Predicted33,Predicted34,Predicted35,Predicted36,Predicted37,Predicted38,Predicted39,Predicted40,Predicted41,Predicted42,Predicted43,Predicted44,Predicted45,Predicted46,Predicted47,Predicted48,Predicted49,Predicted50,Predicted51,Predicted52,Predicted53,Predicted54,Predicted55,Predicted56,Predicted57,Predicted58,Predicted59,Predicted60,Predicted61,Predicted62,Predicted63,Predicted64,Predicted65,Predicted66,Predicted67,Predicted68,Predicted69"
  cat(solution_header, '\n', file=output_file, sep="")
  prediction_df <- data.frame(prediction_matrix)
  write.table(prediction_df, file=output_file, append=TRUE, col.names=FALSE, sep=',', quote=FALSE, row.names=trainSmall$Id)
}

write_solution_to_csv(trainSmall$Prediction[,], 'test_solution.csv')

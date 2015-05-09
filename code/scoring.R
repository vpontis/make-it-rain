ScoreRow <- function(prediction, actual) {
  # we subtract 1 off of n because seq_along is 1-indexed
  mapping <- mapply(function(p, n) {
    return((p - Heaviside(n - 1 - actual))^2)
  }, prediction, seq_along(prediction))
  result <- sum(mapping)/70
  return(result)
}

ScoreAllRows <- function(predictions, expected) {
  # Computes the score over multiple rows
  #
  # Args:
  #   predictions: vector of predictions where each prediction is a length 70 probability vector of different rain fall
  #   actual: the actual rainfall for each measurement, should be same length as predictions
  #
  # Returns:
  #   The score of the predictions

  score_by_prediction <- mapply(ScoreRow, predictions, expected)
  overall_score <- sum(score_by_prediction) / length(predictions)
  return(overall_score)
}

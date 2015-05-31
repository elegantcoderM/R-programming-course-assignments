best <- function(state, outcome) {
  ## Read outcome data 11 -- HA, 17 -- HF, 23 -- Pn
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!is.element(state, data[,7])) {
    stop("invalid state")
  }
  result <- NULL
  minIndices <- NULL

  if (outcome == "heart attack") {
    state_data <- data[which(data$State == state &
                               !is.na(as.numeric(data[,11]))),]
    minIndices <- which(as.numeric(state_data[,11]) ==
                          min(as.numeric(state_data[,11]), na.rm = TRUE))
  } else if (outcome == "heart failure") {
    state_data <- data[which(data$State == state &
                               !is.na(as.numeric(data[,17]))),]
    minIndices <- which(as.numeric(state_data[,17]) ==
                          min(as.numeric(state_data[,17]), na.rm = TRUE))
  } else if (outcome == "pneumonia") {
    state_data <- data[which(data$State == state &
                               !is.na(as.numeric(data[,23]))),]
    minIndices <- which(as.numeric(state_data[,23]) ==
                          min(as.numeric(state_data[,23]), na.rm = TRUE))
  } else {
    stop("invalid outcome")
  }
  # Sort the hospital names in case of a tie and get the first one
  result <- sort(state_data[minIndices,2])[1]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  result
}
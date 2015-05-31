rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!is.element(state, data[,7])) {
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
    outcome_col <- 11
  } else if (outcome == "heart failure") {
    outcome_col <- 17
  } else if (outcome == "pneumonia") {
    outcome_col <- 23
  } else {
    stop("invalid outcome")
  }
  
  ## get the valid state data we are interested in
  state_data <- data[which(data$State == state &
                             !is.na(as.numeric(data[,outcome_col]))),]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  attach(state_data)
  sorted <- state_data[order(as.numeric(state_data[,outcome_col]),
                             state_data[,2]),]
  if ("best" == num) {
    result <- sorted[1,2]
  } else if ("worst" == num) {
    result <- sorted[nrow(state_data),2]
  } else {
    result <- sorted[num,2]
  }
  detach(state_data)
  result
}
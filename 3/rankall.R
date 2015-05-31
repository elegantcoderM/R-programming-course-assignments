rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (outcome == "heart attack") {
    outcome_col <- 11
  } else if (outcome == "heart failure") {
    outcome_col <- 17
  } else if (outcome == "pneumonia") {
    outcome_col <- 23
  } else {
    stop("invalid outcome")
  }

  ## For each state, find the hospital of the given rank
  all_states <- unique(data[,7])
  num_states <- length(all_states)
  result <- data.frame(hospital = NA, state = NA)
  for (i in 1:num_states) {
    ## get the valid state data we are interested in
    state_data <- data[which(data$State == all_states[i] &
                               !is.na(as.numeric(data[,outcome_col]))),]
    attach(state_data)
    sorted <- state_data[order(as.numeric(state_data[,outcome_col]),
                               state_data[,2]),]
    if ("best" == num) {
      result[i,1] <- sorted[1,2]
    } else if ("worst" == num) {
      result[i,1] <- sorted[nrow(state_data),2]
    } else {
      result[i,1] <- sorted[num,2]
    }
    result[i,2] <- all_states[i]
    detach(state_data)
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result
}
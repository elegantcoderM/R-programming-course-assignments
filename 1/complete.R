complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  numFiles = length(id)
  result <- data.frame(id = rep(NA, numFiles), nobs = rep(NA, numFiles))
  
  for (i in 1:numFiles) {
    # Read the file
    filenum <- formatC(id[i], width = 3, flag = "0")
    filename <- paste(directory, "/", filenum, ".csv", sep = '')
    data <- read.csv(filename)
    
    # Get num complete cases
    numCompleteCases <- nrow(data[complete.cases(data),])
    
    # Add to the result variable
    result[i, 1] <- id[i]
    result[i, 2] <- numCompleteCases
  }
  result
}
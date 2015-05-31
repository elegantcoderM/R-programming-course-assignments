corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # Get all files in the directory
  fileNames <- list.files("specdata", pattern = "*.csv", full.names = TRUE)
  numFiles <- length(fileNames)
  
  # Calculate the correlations
  result <- c()
  for (i in 1:numFiles) {
    if ((complete(directory, i)$nobs) > threshold) {
      data <- read.csv(fileNames[i])
      sulfate <- data$sulfate
      nitrate <- data$nitrate
      result <- c(result, cor(sulfate, nitrate, use = "complete.obs"))
    }
  }
  result
}
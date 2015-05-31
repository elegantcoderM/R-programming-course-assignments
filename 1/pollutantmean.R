pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  # Loop through all the necessary files
  numFiles <- length(id)
  sum <- 0
  weight <- 0
  for (i in 1:numFiles) {
    # Read the file
    filenum <- formatC(id[i], width = 3, flag = "0")
    filename <- paste(directory, "/", filenum, ".csv", sep = '')
    data <- read.csv(filename)
    
    # Sum the non NA values
    weight = weight + length(data[!is.na(data[[pollutant]]), pollutant])
    sum = sum + sum(data[!is.na(data[[pollutant]]), pollutant])
  }
  mean <- sum/weight
}
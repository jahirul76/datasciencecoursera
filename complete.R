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
  
  data <- loadcsv(directory)
  cc <- complete.cases(data) 
  data_complete <- data[cc,]
  
  nobs <- rep(id) 
  count <- 1
  for(i in id) {
    nobs[count] <- nrow(data_complete[data_complete$ID == i,])
    count <- count + 1
  
  }
  #print(nobs)
  
  df <- data.frame(id, nobs) 
  
}


loadcsv <- function(directory) { 
  ## directory is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## Load all 'csv' files in the directory 
  ## rbind data.frame to make single table
  files <- dir(directory, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}
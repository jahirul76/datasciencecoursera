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



	## Load all monitoring data in to memory.
	## Calculate mean by subsetting on ID and pollutant columns; remove NA 
	## values from mean calculation.

	if(pollutant == "sulfate" | pollutant == "nitrate") {
		data <- loadcsv(directory)		
		m <- mean(data[data$ID %in% id, pollutant], na.rm = TRUE)
	}
	else {
		warning("param pollutant must be either 'sulfate' or 'nitrate'")
		m <- NA
	}

	m

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

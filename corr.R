corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## get 'complete' case count
  ## subset c_count by threshold
  ## load data
  ## remove incomplete cases
  ## for each cor (sulfate, nitrate)
  
  
  c_count <- complete(directory)
  
  th_mon <- c_count[c_count$nobs > threshold, 'id']  
  #print(length(th_mon))
  
  data <- loadcsv(directory)  
  #print(nrow(data))
  
  data <- data[complete.cases(data),]  
  #print(nrow(data))
  
  cr <- rep(0, length(th_mon))
  cnt <- 1
  
  for(mon in  th_mon) {
    cr[cnt] <- cor(data[data$ID==mon, c('sulfate')], data[data$ID==mon, c('nitrate')])
    cnt <- cnt + 1
  }
        
  cr
}



corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  correlations <- numeric()
  files <- list.files(path = directory, full.names=TRUE)
  for (file in files){
    content <- read.csv(file)
    ok <- complete.cases(content[,c("sulfate","nitrate")])
    if (sum(ok) > threshold){
      complete <- content[ok,]
      correlations <- append(correlations, cor(complete$sulfate, complete$nitrate))
      #print(correlations)
    } 
  }
  correlations
}

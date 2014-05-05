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
  
  files <- sprintf("./%s/%03d.csv",directory,id)
  contents <- lapply(files,read.csv)
  all <- do.call("rbind",contents)  #deconstruct contents so that it fits ellipsis
  # now filter out the complete measurements
  completes <- all[complete.cases(all[, c("sulfate", "nitrate")]), ]
  # and group them on ID
  counts <- aggregate( sulfate ~ ID, completes, length)  #loses ordering
  # reorder them
  orderedcounts <- counts[match(id,counts$ID),]
  names(orderedcounts) <- c("id","nobs")
  orderedcounts
}

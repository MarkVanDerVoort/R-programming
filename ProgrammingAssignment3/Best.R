as.name <- function(outcome){
  # Capitalized lower case words
  capitalized <- gsub("([a-zA-Z])([a-zA-Z]+)","\\U\\1\\L\\2",outcome, perl=TRUE)
  # with spaces replaced by a single dot
  dotted <- gsub("( )+","\\.", capitalized)
  paste("Hospital.30.Day.Death..Mortality..Rates.from.",dotted,sep="") 
}

## Returns the dataset
## Performs validation, stopping if not valid
##   state     is a value in the input
##   outcome   occurs as a 30daypercentile column name  
validated.input <- function(outcome, state=NA){
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character")
  if (!is.na(state)){      #only validate if state is explicitly given
    if (!(state %in% outcomes$State)) {
      stop("invalid state") 
    }
  }
  if (!(as.name(outcome) %in% names(outcomes))){
    stop("invalid outcome")
  }
  outcomes
}

#Non generic narrowing function, use for debugging..
# narrwos the dataset, and performs a numeric cast.
debug.narrow <- function(hospitals, outcome){
  full.outcome <- as.name(outcome)
  hospitals <- hospitals[ , c("State","Hospital.Name",full.outcome)]
  outcome.col <- as.numeric(hospitals[ , full.outcome])
  hospitals[ , full.outcome] <- outcome.col
  hospitals  
}


## Returns an ordered, narrowed dataset
## ordering by outcome, name
ordered <- function(hospitals,outcome){
  full.outcome <- as.name(outcome)
  hospitals <- hospitals[ , c("State","Hospital.Name",full.outcome)]
  outcome.col <- as.numeric(hospitals[ , full.outcome])
  hospitals[ , full.outcome] <- outcome.col
  row.ordering <- order(hospitals[ , full.outcome], 
                        hospitals[ , "Hospital.Name"], 
                        na.last=NA)
  ordered.hospitals <- hospitals[ row.ordering , ]  
  ordered.hospitals
}

## Returns the best hospital for `outcome` within the `state`
best <- function(state,outcome){
  all.hospitals <- validated.input(outcome,state)
  state.hospitals <- subset(all.hospitals, all.hospitals$State == state)  #maybe use index notation
  ordered.hospitals <- ordered(state.hospitals, outcome)
  ordered.hospitals$Hospital.Name[1]
}
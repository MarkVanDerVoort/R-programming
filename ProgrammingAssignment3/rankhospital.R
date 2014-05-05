source("best.r")

## Returns a ranknumber for a qualitative rank {best,worst,1:n}
rank.quantitative <- function(df,rank){
  # get the rank now..
  if (rank == "best"){
    eff.rank <- 1
  } else if (rank == "worst"){
    eff.rank <- nrow(df)
  } else {
    eff.rank <- rank
  }  
  eff.rank
}

## Returns the hospital ranked `rank`, for give `state` and `outcome`
rankhospital <- function(state, outcome, rank = "best") {
  all.hospitals <- validated.input(outcome,state)
  state.hospitals <- subset(all.hospitals, all.hospitals$State == state)
  ordered.hospitals <- ordered(state.hospitals, outcome)
  
  eff.rank <- rank.quantitative(ordered.hospitals,rank)
  ordered.hospitals$Hospital.Name[eff.rank]
}

## Returns a selector function
## which when applied to a dataframe returns the hospital ranked `rank` for an `outcome`
fn.rank <- function(rank,outcome){
  #for rankall we probably need a function returning a function
  #to be used in aggregate
  a <- function(df){
    eff.rank <- rank.quantitative(df,rank)
    ordered(df,outcome)[eff.rank , ] 
  }
  a
}

rankall <- function(outcome, num = "best") {
  all.hospitals <- validated.input(outcome)
  all.hospitals <- debug.narrow(all.hospitals, outcome)
  split.hospitals <- split(all.hospitals, all.hospitals$State)
  tmp2 <- sapply(split.hospitals, fn.rank(num,outcome))
  tmpBy <- by(all.hospitals, all.hospitals$State, fn.rank(num,outcome))
  df.out <- do.call(rbind,tmpBy)
  df <- df.out[ , c(2,1)]
  names(df) <- c("hospital","state")
  df
}  

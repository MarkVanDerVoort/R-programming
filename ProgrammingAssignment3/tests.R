source('rankhospital.R')

best("SC", "heart attack") == "MUSC MEDICAL CENTER"
best("NY", "pneumonia")    == "MAIMONIDES MEDICAL CENTER"
#is.na( best("NN", "pneumonia") )

rankhospital("NC", "heart attack", "worst") == "WAYNE MEMORIAL HOSPITAL"
rankhospital("WA", "heart attack", 7)       == "YAKIMA VALLEY MEMORIAL HOSPITAL"
is.na( rankhospital("WA", "pneumonia", 1000) )
#rankhospital("NY", "heart attak", 7)

# 8:10
should.equal <- function(a,b){
  if (a==b){
    TRUE
  } else {
    print(paste( a, " =/= ",  b))
    FALSE
  }
}

equiv <- function(state,outcome,rank){
  should.equal( rankhospital(state,outcome,rank) , rankall(outcome,rank)[state,"hospital"])
}

equiv("AK","heart attack", 4)

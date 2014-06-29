rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states <- unique(data$State)
  if (!(state %in% states)) {
    stop("invalid state")
  }
  resolved_outcome <- switch (outcome,
                              "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                              "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                              "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (is.null(resolved_outcome)) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  all_hospital_in_state <- data[data$State == state, c("Hospital.Name", resolved_outcome)]
  all_hospital_in_state[,2] <- as.numeric(all_hospital_in_state[,2])
  all_hospital_in_state <- all_hospital_in_state[complete.cases(all_hospital_in_state),]
  ordered_by_outcome <- order(all_hospital_in_state[resolved_outcome], 
                              all_hospital_in_state$Hospital.Name)
  
  rank <- if (num == "best") {
    1
  } else if (num == "worst") {
    length(ordered_by_outcome)
  } else if (num <= length(ordered_by_outcome)) {
    num
  } else {
    return(NA)
  }
  as.character(all_hospital_in_state$Hospital.Name[ordered_by_outcome[rank]])
}
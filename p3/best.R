best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  all_hospital_in_state <- data[data$State == state, c("Hospital.Name", resolved_outcome)]
  all_hospital_in_state[,2] <- as.numeric(all_hospital_in_state[,2])
  ordered_by_outcome <- order(all_hospital_in_state[resolved_outcome], 
                              all_hospital_in_state$Hospital.Name)
  as.character(all_hospital_in_state$Hospital.Name[ordered_by_outcome[1]])
}
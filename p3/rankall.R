source("rankhospital.R")

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that outcome is valid
  resolved_outcome <- switch (outcome,
                              "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                              "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                              "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (is.null(resolved_outcome)) {
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data_by_state <- split(data[, c("Hospital.Name", "State", resolved_outcome)], data$State)
  
  rank_hospital <- function(state_data, num) {
    state_data[,resolved_outcome] <- as.numeric(state_data[,resolved_outcome])
    state_data <- state_data[complete.cases(state_data),]
    ordered_state_data <- order(state_data[resolved_outcome], state_data$Hospital.Name)  
    if (num == "best") {
      state_data$Hospital.Name[ordered_state_data[1]]
    } else if (num == "worst") {
      state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
    } else if (num <= length(ordered_state_data)) {
      state_data$Hospital.Name[ordered_state_data[num]]
    } else {
      return(NA)
    }
  }
  
  pre_result <- lapply(data_by_state, rank_hospital, num)
  
  data.frame(hospital = unlist(pre_result), state = names(pre_result), row.names = names(pre_result))
}
best <- function(state, diagnosis) {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[outcome == "Not Available"] <- NA
  states <- outcome[, 7]
  state_ind <- which(states == state)
  if(length(state_ind) == 0) stop("invalid state")
  if(diagnosis == "heart attack") ind <- 11
  else if(diagnosis == "heart failure") ind <- 17
  else if(diagnosis == "pneumonia") ind <- 23
  else stop("invalid outcome")
  rate <- min(as.numeric(outcome[state_ind, ind]), na.rm = TRUE) 
  indices <- which(as.numeric(outcome[state_ind, ind]) == rate)
  names <- sort(outcome[state_ind[indices], 2])
  names[1]      
}


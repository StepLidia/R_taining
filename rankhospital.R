rankhospital <- function(state, diagnosis, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[outcome == "Not Available"] <- NA
  states <- outcome[, 7]
  state_ind <- which(states == state)
  if(length(state_ind) == 0) stop("invalid state")
  if(diagnosis == "heart attack") ind <- 11
  else if(diagnosis == "heart failure") ind <- 17
  else if(diagnosis == "pneumonia") ind <- 23
  else stop("invalid outcome")
  
  rate <- order(as.numeric(outcome[state_ind, ind]), outcome[state_ind, 2], na.last = NA)
  if(num == "best") answer <- outcome[state_ind[rate[1]], 2]
  else if(num == "worst") answer <- outcome[state_ind[tail(rate, 1)], 2]
  else if(num > length(rate)) answer <- NA
  else answer <- outcome[state_ind[rate[num]], 2]
  answer
}
rankall <- function(diagnosis, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[outcome == "Not Available"] <- NA
  states <- unique(outcome[, 7])
  
  if(diagnosis == "heart attack") ind <- 11
  else if(diagnosis == "heart failure") ind <- 17
  else if(diagnosis == "pneumonia") ind <- 23
  else stop("invalid outcome")
  
  answer <- array(0, length(states))
  i <- 1
  for(s in states) {
    state_ind = which(outcome[, 7] == s) #for each state find hospital ranking
    rate <- order(as.numeric(outcome[state_ind, ind]), outcome[state_ind, 2], na.last = NA)
    if(num == "best") answer[i] <- outcome[state_ind[rate[1]], 2]
    else if(num == "worst") answer[i] <- outcome[state_ind[tail(rate, 1)], 2]
    else if(num > length(rate)) answer[i] <- NA
    else answer[i] <- outcome[state_ind[rate[num]], 2]
    i <- i + 1
  }
  ind <- order(states)
  ranking <- data.frame(answer[ind], states[ind])
  colnames(ranking) <- c("hospital", "state")
  ranking
}
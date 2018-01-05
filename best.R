# Function of of 2-character state name abbreviation and outcome name. 
# Reads the outcome-of-care-measures, returns hospital name (character vector) 
# with lowest 30-day mortality in the state for a outcome in heart attack, heart 
# failure, or pneumonia. 

best <- function(state, outcome){
      # Read outcome data
      dataRaw <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character", 
                          na.strings = "Not Available", 
                          stringsAsFactors = FALSE)
      data <- dataRaw[, c(2, 7, 11, 17, 23)]
      # format extracted data frame: set column names
      names(data) <- c("hospital", "state", "heart attack", "heart failure", 
                       "pneumonia")
      # change unwanted character class to numeric
      data[,3] <- as.numeric(data[,3])
      data[,4] <- as.numeric(data[,4])
      data[,5] <- as.numeric(data[,5])
      
      # check valid state
      if(any(state == data$state))
            dataState <- data[(data$state == state),]
      else      
            stop("invalid state")
      # check valid outcome
      if(outcome == names(data)[3])
            outtemp <- 3
      else if(outcome == names(data)[4])
            outtemp <- 4
      else if(outcome == names(data)[5])
            outtemp <- 5
      else
            stop("invalid outcome")

      # sort by outcome value, the hospital names
      sort <- dataState[order(dataState[outtemp], dataState[1]),]
      # return hospital name
      sort[1,1]
}
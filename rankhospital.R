# ranking hospitals by outcome in a state, return hospital name of hospital
# at selected ranking place in selected state

rankhospital <- function(state, outcome, num = "best"){
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
      
      # sort by outcome value, excluding NAs on that outcome
      dataReduce <- dataState[!is.na(dataState[, outtemp]),]
      sort <- dataReduce[order(dataReduce[outtemp], dataReduce[1]),]
     
      # check for num
      if(num == "best")
            index = 1
      else if(num == "worst")
            index = nrow(sort)
      else if(num %% 1 == 0)
            index = num
      
      # return hospital name
      sort[index,1]
}
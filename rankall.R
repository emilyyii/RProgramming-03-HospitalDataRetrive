# ranking hospitals by outcome in all states, output data frame of hospital 
# names at chosen ranking place in each states respectively

rankall <- function(outcome, num = "best"){
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
      
      # check valid outcome
      if(outcome == names(data)[3])
            outtemp <- 3
      else if(outcome == names(data)[4])
            outtemp <- 4
      else if(outcome == names(data)[5])
            outtemp <- 5
      else
            stop("invalid outcome")
      
      # excludig NAs
      dataReduce <- data[!is.na(data[, outtemp]),]
      # sort by state, outcome value, then hospital names
      sorttemp <- dataReduce[order(dataReduce[2], dataReduce[outtemp], 
                                   dataReduce[1]),]
      # split by states
      sort <- split(sorttemp, as.factor(sorttemp$state))
      # initalize vector for hospitals, length = number of states
      hospNam <- vector("character", length = length(sort))
      
      # retrive hospital name for each state for different num
      if(num == "best"){
            for(i in 1:length(sort))
                  # index 1 in ith state, 1 = hospital name
                  hospNam[i] <- sort[[i]][1,1] 
      }
      else if(num == "worst"){
            for(i in 1:length(sort)){
                  index <- nrow(sort[[i]])
                  # ith state, 1 = hospital name, index = ranking
                  hospNam[i] <- sort[[i]][index,1] 
            }
      }
       else if(num %% 1 == 0){
            for(i in 1:length(sort))
                  # numth in ith state, 1 = hospital name
                  hospNam[i] <- sort[[i]][num,1] 
      }
      # get statenames
      stateName <- names(sort)
      # combine vectors into named data frame
      result <- data.frame(hospital = hospNam, state = stateName,
                           row.names = stateName)
      
}
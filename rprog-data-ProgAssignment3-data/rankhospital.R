## The function reads the outcome-of-care-measures.csv file and 
## returns a character vector with the name of the hospital that has 
## the ranking specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5) would return a character vector 
## containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. 

## The function called rankhospital that takes three arguments: 
## the 2-character abbreviated name of a state (state), an outcome (outcome),
## and the ranking of a hospital in that state for that outcome (num

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # --- Coerce character into numeric
        data[,11] <- suppressWarnings(as.numeric(data[,11]))
        data[,17] <- suppressWarnings(as.numeric(data[,17]))
        data[,23] <- suppressWarnings(as.numeric(data[,23]))
        
        # --- create variables to store valid outcome, valid state and outcome column
        valid_state <- as.character(unique(data$State))
        valid_outcome <- c("heart attack", "heart failure", "pneumonia") 
        outcome_column <- c(11, 17, 23)
        
        # --- translate outcome to the selected column to find the
        # hospital that has the best mortality rate in the selected outcome group
        if (outcome == "heart attack") outcome_column = 3
        if (outcome == "heart failure") outcome_column = 4
        if (outcome == "pneumonia") outcome_column = 5
        
        # --- Check that state and outcome are valid
        if ((state %in% valid_state) == FALSE) stop ("Invalid State")
        if ((outcome %in% valid_outcome) == FALSE) stop ("Invalid Outcome")
        
        # --- Create a new data frame with only the require columns
        #  [1] "Hospital.Name" 
        #  [2] "State"                                                     
        #  [3] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        #  [4] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        #  [5] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"   
        data <- data[-c(1, 3:6, 8:10, 12:16, 18:22, 24:47)]
        
        # --- Filter data by  the selected state input
        statedata <- data [grep(state,data$State),]
        
        # --- Order statedata in increasing order 
        #       by the selected outcome column 
        order_statedata <- statedata[order(decreasing = FALSE, 
                                           statedata[,outcome_column],
                                           statedata[,1],
                                           na.last = NA),]
        if (num == "best")
                num <- 1
        if (num == "worst")
                num <- nrow(order_statedata)
        suppressWarnings(rank_num <- as.numeric(num))
        #print(rank_num)
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        return(order_statedata[rank_num,]$Hospital.Name)
}
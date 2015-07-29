## function called best takes two arguments: 
## the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file
## and returns a character vector with the name of the hospital that
## has the best (i.e. lowest) 30-day mortality for the specified outcome ##
## in that state. The hospital name is the name provided in the 
## Hospital.Name variable. The outcomes can be one of 
## “heart attack”, “heart failure”, or “pneumonia”. 
## Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings

best <- function (state, outcome) {
        # --- Read outcome data
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
                                           na.last = NA),]
        # --- Return hospital name with the lowest 30day death rate by
        #       selecting the 1st row and 1st column
        return(order_statedata[1,1])
}


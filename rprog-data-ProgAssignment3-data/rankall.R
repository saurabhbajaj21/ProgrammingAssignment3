## the function call rankall("heart attack", "best") would return a 
## data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. 

## The function should return a value for every state (some may be NA). 

## The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains
## the 2-character abbreviation for the state name. 

## Hospitals that do not have data on a particular outcome should be excluded 
## from the set of hospitals when deciding the rankings

rankall <- function (outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        output_data <- vector()
        
        # --- Coerce character into numeric
        data[,11] <- suppressWarnings(as.numeric(data[,11]))
        data[,17] <- suppressWarnings(as.numeric(data[,17]))
        data[,23] <- suppressWarnings(as.numeric(data[,23]))
        
        # --- create variables to store valid outcome, valid state and outcome column
        valid_outcome <- c("heart attack", "heart failure", "pneumonia") 
        outcome_column <- c(11, 17, 23)
        valid_state <- unique(data$State)
        valid_state <- valid_state[order(decreasing = FALSE, valid_state)]
        #valid_state <- c("WY")
        
        # --- translate outcome to the selected column to find the
        # hospital that has the best mortality rate in the selected outcome group
        if (outcome == "heart attack") outcome_column = 3
        if (outcome == "heart failure") outcome_column = 4
        if (outcome == "pneumonia") outcome_column = 5
        
        # --- Check that outcome are valid "heart attack", "heart failure", "pneumonia"
        if ((outcome %in% valid_outcome) == FALSE) stop ("Invalid Outcome")
        
        # --- Create a new data frame with only the require columns
        #  [1] "Hospital.Name" 
        #  [2] "State"                                                     
        #  [3] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        #  [4] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        #  [5] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"   
        data <- data[-c(1, 3:6, 8:10, 12:16, 18:22, 24:47)]
        
        # --- Filter data by  the selected state input
        
        for (i in 1:length(valid_state)) { 
                # -- Sort ascending on outcome_column
                # in case of a tie sort on hospital name -> statedata[,1]
                statedata <- data[grep(valid_state[i],data$State),]
                order_statedata <- statedata[order(decreasing = FALSE, 
                                                   statedata[,outcome_column],
                                                   statedata[,1],
                                                   na.last = NA),]
               #Get ranking of the hospital to be selected from the input num variable 
                if (num == "best")  rank_num <- 1
                else if (num == "worst") rank_num <- nrow(order_statedata)
                else rank_num <- num
                suppressWarnings(rank_num <- as.numeric(rank_num))
                
                # --Return hospital name in that state with the given rank
                ## 30-day death rate
                output_data <- append(output_data, as.character(order_statedata[rank_num,1]))
                output_data <- append(output_data,as.character(valid_state[i]))
        }
        
        # --Just because it's simpler to generate a matrix rather than a data frame, 
        #       we generate it first and convert it to data frame immediatly after.
        output_data <- as.data.frame(matrix(output_data, nrow = length(valid_state), 
                                            ncol = 2, 
                                            byrow = TRUE))
   
        colnames(output_data) <- c("hospital","state")
        rownames(output_data) <- valid_state
        return(output_data)
        
}
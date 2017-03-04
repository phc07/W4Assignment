best <- function(state, outcome){
        
        ## Read outcome data
        oc <- read.csv("outcome-of-care-measure.csv", colClasses = "character")
        
        ## Check State and Outcome is valid
        if (! state %in% outcome$State) {print("state is not valid, try again")}
        else if (outcome == "heart attack") {sick <- oc$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack}
        else if (outcome == "heart failure") {sick <- oc$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure}
        else if (outcome == "pneumonia") {sick <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia}
        else {print("sickness is not valid, try again")}
        
        ## Return hospital name in that state with lowest 30-day death
        
        

}
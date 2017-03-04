best <- function(state, outcome){
        
        ## Read outcome data
        oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check State and Outcome is valid
        if (! state %in% oc$State) {
                print("state is not valid, try again")
                stop()
                }
        else if (outcome == "heart attack") {sick <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
        else if (outcome == "heart failure") {sick <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
        else if (outcome == "pneumonia") {sick <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
        else {
                print("sickness is not valid, try again")
                stop()}
        sick
        ## Return hospital name in that state with lowest 30-day death
        r_oc <- oc[,order(oc[,sick])]
        r_oc

}
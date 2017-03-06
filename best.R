best <- function(state, outcome){
        
        ## Read outcome data
        oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check State and Outcome is valid
        if (! state %in% oc$State) {
                print("state is not valid, try again")
                stop()
                }
        else if (outcome == "heart attack") {r_oc <- oc[,c(2,9,13)]}
        #column 13 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        else if (outcome == "heart failure") {r_oc <- oc[,c(2,9,19)]}
        #column 19 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        else if (outcome == "pneumonia") {r_oc <- oc[,c(2,9,25)]}
        #column 25 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        else {
                print("sickness is not valid, try again")
                stop()}
        ## Return hospital name in that state with lowest 30-day death
        r_oc <- subset(r_oc, r_oc[,2]!="Not Available")
        o_oc <- r_oc[order(r_oc,r_oc[,2])]
        head(o_oc)
        
        

}
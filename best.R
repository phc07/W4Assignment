best <- function(state, outcome){
        
        ## Read outcome data
        oc <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        ## Check State and Outcome is valid
        if (! state %in% oc$State) {
                print("state is not valid, try again")
                stop()
        }
        oc <- subset(oc,oc$State==state)
        if (outcome == "heart attack") {r_oc <- oc[,c(2,7,11)]}
        #column 11 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        else if (outcome == "heart failure") {r_oc <- oc[,c(2,7,17)]}
        #column 19 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        else if (outcome == "pneumonia") {r_oc <- oc[,c(2,7,23)]}
        #column 25 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        else {
                print("sickness is not valid, try again")
                stop()}
        ## Return hospital name in that state with lowest 30-day death
        r_oc <- subset(na.omit(r_oc))
        #str(r_oc)
        c_name <- names(r_oc[3])
        o_oc <- r_oc[order(r_oc[3]),]
        o_oc[1,1]
        
        

}
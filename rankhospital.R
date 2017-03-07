rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        oc <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        ## Check that state and outcome are valid
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
        
        r_oc <- subset(na.omit(r_oc))

        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        c_name <- names(r_oc[3])
        o_oc <- r_oc[order(r_oc[3],r_oc[2]),]
        nrow(o_oc)
        
        if (num=="best"){r <- 1}
        else if (num=="worst") {r <- nrow(r_oc)}
        else {r <- num}
        
        #o_oc[1,40]
        o_oc[r,1]
}

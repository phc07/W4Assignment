rankall <- function(outcome, num = "best") {
        ## Read outcome data
        oc <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        ## Check that state and outcome are valid
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
        
        #f_oc <- o_oc(,[1], o_oc[2],o_oc[r,c(1,2)])
        #f_oc <- do.call(rbind,by(o_oc,o_oc$State,function(x) x[r,]))
        f_oc <- ddply(o_oc,o_oc[,2],function(r) o_oc[r,])
        f_oc
}
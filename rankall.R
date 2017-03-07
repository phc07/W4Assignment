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
        
        o_oc <- r_oc[order(r_oc[2],r_oc[3]),]
        
        if (num=="best"){
                r <- 1
                f_oc <- do.call(rbind,by(o_oc,o_oc$State,function(x) x[r,1:2]))
                }
        else if (num=="worst") {
                t_oc <- aggregate(o_oc,list(o_oc[,2]),tail,1)
                f_oc <- t_oc[,1:2]
                }
        else {
                r <- num
                f_oc <- do.call(rbind,by(o_oc,o_oc$State,function(x) x[r,1:2]))
                }
        
        #f_oc <- o_oc(,[1], o_oc[2],o_oc[r,c(1,2)])
        #f_oc <- do.call(rbind,by(o_oc,o_oc$State,function(x) x[r,1:2]))
        #f_oc <- tapply(r_oc, r_oc[,2],function(r) r_oc[r,])
        f_oc
}
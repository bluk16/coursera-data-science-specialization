best <- function(state,outcome) {
    i <- 0
    options(warn=-1)
    rawFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if(!any(state == rawFile$State)) {
        stop('invalid state')
    }
    if(outcome == 'heart attack') {
        i <- 11
    }
    else if(outcome == 'heart failure') {
        i <- 17
    }
    else if(outcome == 'pneumonia') {
        i <- 23
    }
    else {
        stop('invalid outcome')
    }    
    
    rawFile[,i] <- as.numeric(rawFile[, i])
    rawFile <- rawFile[complete.cases(rawFile[, i]), ]
    data <- rawFile[ ,c(2,7,i)]
    data <- data[data$State == state, ]
    result <- data[which.min(data[, 3]),1]
    result
}

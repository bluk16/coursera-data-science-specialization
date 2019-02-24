corr <- function(directory, threshold = 0) {
    result <- 0
    complete_cases <- complete(directory)
    complete_cases <- complete_cases[complete_cases$nobs > threshold, ]
    allCleanCases <- data.frame()
    ids <- complete_cases[ ,1]

    if(length(ids) > 0) {
        for(i in ids) {
            filePathStr <- paste(c(getwd(), "/", directory, "/", formatC(i, width=3, flag="0"), ".csv"), collapse = "")
            rawFile = read.csv(filePathStr, header=TRUE)
            allCleanCases <- rbind(allCleanCases,rawFile[complete.cases(rawFile), ])
        }
        result = cor(allCleanCases[ ,2], allCleanCases[ ,3])
    } else {
        result = 0
    }
    result
}
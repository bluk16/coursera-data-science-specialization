complete <- function(directory,id=1:332) {
    ids <- vector()
    nobs <- vector()

    for(i in id) {
        filePathStr <- paste(c(getwd(), "/", directory, "/", formatC(i, width=3, flag="0"), ".csv"), collapse = "")
        rawFile = read.csv(filePathStr, header=TRUE)

        n <- 0 # counter for number completed observed case

        for(j in 1:nrow(rawFile)) {
            row <- rawFile[j, ]
            if(sum(is.na(row)) == 0) {
                n <- n + 1
            }
        }

        if(n > 0) {
            ids <- c(ids,i)
            nobs <- c(nobs,n)
        }
    }
    resultMatrix <- data.frame(id=ids,nobs=nobs)
    resultMatrix
}
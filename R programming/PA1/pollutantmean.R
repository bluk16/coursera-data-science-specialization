pollutantmean <- function(directory, pollutant, id = 1:332) {
    total <- 0
    n <- 0

    for (i in id) {
        filePathStr <- paste(c(getwd(), "/", directory, "/", formatC(i, width=3, flag="0"), ".csv"), collapse = "")
        rawFile = read.csv(filePathStr, header=TRUE)

        selectedRow <- rawFile[ ,pollutant]
        cleanedRow <- selectedRow[!is.na(selectedRow)]
        n <- n + length(cleanedRow)
        total <- total + sum(cleanedRow)
    }
    total/n # returning mean
}

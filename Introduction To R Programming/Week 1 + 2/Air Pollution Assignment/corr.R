library(plyr)
library(readr)

corr <- function(directory, threshold = 0) {
    # 'directory' is a character vector of length 1 indicating the location of the CSV files.
    #
    # 'threshold' is a numeric vector of length 1 indicating the number of completely observed
    # observations (on all variables) required to compute the correlation between nitrate and
    # sulfate; the default is 0.
    #
    # Return a numeric vector of correlations.
    # NOTE: DO NOT ROUND THE RESULT

    ## Get list of CSV files from given directory.
    filesList = list.files(path = directory, pattern = "*.csv", full.names = T)

    ## Import the CSV data into a data frame object.
    dat_csv = ldply(filesList, read_csv)

    # Get number of complete observations for each sensor ID in a data frame
    completeVec = complete(directory)

    # Subset complete vector to only those sensors which fit threshold value
    fitThreshold = completeVec[completeVec$nobs > threshold,]

    # Initialize return vector
    returnVector = numeric()

    # Iterate over all sensor IDs that fit the threshold values
    for(sensorId in fitThreshold$id) {
        filteredById = dat_csv[dat_csv$ID == sensorId,]
        completeVec = filteredById[complete.cases(filteredById),]
        returnVector = c(returnVector, cor(completeVec$sulfate, completeVec$nitrate))
    }

    returnVector
}
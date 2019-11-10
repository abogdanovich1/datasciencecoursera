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
    
    # Iterate over all the sensorIDs and find which ones fit the threshold
    idsFittingThreshold = vector()
    for(sensorId in 1:nrow(completeVec)) {
        tempFilteredFrame = subset.data.frame(dat_csv, ID %in% sensorId)
        ratio = nrow(tempFilteredFrame)
        if(ratio >= threshold) {
            # This sensor has enough complete observations to warrant a correlation calculation
            idsFittingThreshold = c(idsFittingThreshold, sensorId)
        }
    }

    # Filter the original data frame to only include sensorIDs that fit threshold
    filteredById = subset.data.frame(dat_csv, ID %in% idsFittingThreshold)

    # Initialize return vector
    returnVector = vector()
    # Iterate over each ID that fits threshold, figure out correlation
    for(sensorId in idsFittingThreshold) {
        tempFilteredFrame = subset.data.frame(dat_csv, ID %in% sensorId)
        completeFrame = tempFilteredFrame[complete.cases(tempFilteredFrame),]
        x = completeFrame$sulfate
        y = completeFrame$nitrate
        returnVector = c(returnVector, cor(x = x, y = y))
    }

    returnVector
}
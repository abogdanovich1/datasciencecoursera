library(plyr)
library(readr)

complete <- function(directory, id = 1:332) {
    # 'directory" is a character vector of length 1 indicating the location of the CSV files.
    #
    # 'id' is an integer vector indicating the monitor ID numbers to be used
    #
    # Return a data frame of the form:
    # id nobs
    # 1  117
    # 2  1041
    # ...
    # where 'id' is the monitor ID number, and 'nobs' is the number of complete cases
    
    ## Get list of CSV files from given directory.
    filesList = list.files(path = directory, pattern = "*.csv", full.names = T)
    
    ## Import the CSV data into a data frame object.
    dat_csv = ldply(filesList, read_csv)
    
    ## Filter the data frame to only the sensor IDs we are interested in.
    filteredById = subset.data.frame(dat_csv, ID %in% id)
    
    # Filter the data to only the complete cases in the data frame.
    completeVec = filteredById[complete.cases(filteredById),]
    
    # Create new data frame that will be returned
    returnFrame = data.frame("id" = id, "nobs" = 0)
    
    # Iterate over each ID number, and add number of observations to data frame
    index = 1
    for(sensorId in id) {
        tempFrame = filter(completeVec, ID == sensorId)
        returnFrame[[index, 2]] = nrow(tempFrame)
        index = index + 1
    }
    
    # Return the data frame
    returnFrame
}


library(plyr)
library(readr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # 'directoy' is a character vector of length 1 indicating location of CSV files.
    #
    # 'pollutant' is a character vector of length 1 indicating the name of the 
    #  pollutant for which we will calculate the mean. Either "sulfate" or "nitrate"
    #
    # 'id' is an integer vector indicating the monitor ID numbers to be used
    #
    #  Return the mean of the pollutant across all monitors list in the 'id' vector
    #  (ignoring NA values). DO NOT ROUND THE RESULT
    
    ## Get list of CSV files from given directory.
    filesList = list.files(path = directory, pattern = "*.csv", full.names = T)
    
    ## Import the CSV data into a data frame object.
    dat_csv = ldply(filesList, read_csv)
    
    ## Filter the data frame to only the sensor IDs we are interested in.
    filteredById = subset.data.frame(dat_csv, ID %in% id)
    
    if(pollutant == "sulfate") {
        # We are interested in the mean of the sulfate levels
        sulfateMean = mean(filteredById$sulfate, na.rm = TRUE)
        sulfateMean
    } else {
        # We are interested in the mean of the nitrate levels
        nitrateMean = mean(filteredById$nitrate, na.rm = TRUE)
        nitrateMean
    }
}
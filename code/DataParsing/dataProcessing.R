#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Given a list of character vectors containing the data in each column, processes
#  and cleans the data, and returns a dataframe that can be appended to the final
########################################################################################

source("code/Helper/Helper.R")

NUM_COLUMNS <- 6
FIRST_NUMERIC_COL <- 4
LAST_NUMERIC_COL <- 6

dataProcessing <- function(splitColumns, origCounty, state, origBank) {
    county <- origCounty
    
    # delete unneccessary whitespace data
    for (i in 1:NUM_COLUMNS) {
        col <- splitColumns[[i]]
        whiteSpaces <- col == ""
        splitColumns[[i]] <- splitColumns[[i]][!whiteSpaces]
    }
    
    # determine whether or not a new county begins on this page
    findCounty <- findCounty(splitColumns[[1]])
    if (findCounty[[1]] != -1) { # if there is a new county, delete ****** from first row
        deleteRows <- c(rep(TRUE, length(splitColumns[[1]])))
        deleteRows[findCounty[[1]]] <- FALSE
        deleteRows[findCounty[[1]] + 1] <- FALSE
        deleteRows[findCounty[[1]] - 1] <- FALSE
        splitColumns[[1]] <- splitColumns[[1]][deleteRows]
        county <- findCounty[[2]]
    }
    
    
    # determine whether or not there is a county change on this page
    # meaning data at the top belong to one county, and data at the bottom belong to another
    indexOfTable <- findCountyTotals(splitColumns[[1]])
    if (indexOfTable != -1) {   # if there is a county change, delete county table from all rows
        for (j in 1:NUM_COLUMNS) {
            deleteRows <- c(rep(TRUE, length(splitColumns[[j]])))
            deleteRows[indexOfTable] <- FALSE
            deleteRows[indexOfTable + 1] <- FALSE
            deleteRows[indexOfTable + 2] <- FALSE
            deleteRows[indexOfTable + 3] <- FALSE
            deleteRows[indexOfTable + 4] <- FALSE
            if (j == 1 | j == 4) {
                deleteRows[indexOfTable + 5] <- FALSE
            }
            splitColumns[[j]] <- splitColumns[[j]][deleteRows]
        }
    } # all rows above indexOfTable need origCounty, all rows below need county
    
    
    
    # all column data vectors need to have same length before putting in dataframe
    greatestLength <- 0
    for (i in 1:NUM_COLUMNS) {        # identify the column with the greatest length
        len <- length(splitColumns[[i]])
        if (len > greatestLength) {
            greatestLength <- len
        }
    }
    stringData <- data.frame(Temp = 1:greatestLength)  # put data into a dataframe!
    for (i in 1:NUM_COLUMNS) {
        col = splitColumns[[i]]
        if (length(col) < greatestLength) {
            col <- c(col, rep('#', greatestLength - length(col))) # add '#' characters to fill in any short columns
        }
        stringData[paste0("Col", i)] <- col
    }
    stringData <- stringData[-1]

    # clean data - remove all periods/commas, replace all lone 'o' or 'O' with '0'
    stringData <- cleanData(stringData, FIRST_NUMERIC_COL, LAST_NUMERIC_COL)
    
    # need to convert last 3 columns from string to numeric?
    
    # separating bank and branch names into different columns
    stringData <- separateBanksAndBranches(stringData, origBank)
    
    # add and fill County and State columns
    stringData <- cbind(data.frame("County" = c(rep(county, length(stringData$Bank)))), stringData)
    
    for (i in 1:indexOfTable) {
        stringData$County[i] <- origCounty
    }
    #print(stringData)
    stringData <- cbind(data.frame("State" = c(rep(state, length(stringData$Bank)))), stringData)
    #print(stringData)
    colnames(stringData) <- c("State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
    
    return(stringData)
    
}

#print("------------------ Cleaned Data --------------------")
#print(stringData)

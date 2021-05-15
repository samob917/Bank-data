#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Given a list of character vectors containing the data in each column, processes
#  and cleans the data, and returns a dataframe that can be appended to the final
# Parameters:
#   splitColumns - list of character vectors. each vector contains the entries of a column
#   origCounty - string, county of data at top of page
#   state - string, state of data at top of page
#   origBank - string, bank of data at top of page
########################################################################################

source("code/Helper/Helper.R")

NUM_COLUMNS <- 6
FIRST_NUMERIC_COL <- 4
LAST_NUMERIC_COL <- 6

dataProcessing <- function(splitColumns, origCounty, state, origBank) {
    counties <- vector(mode = "character")
    
    # delete unneccessary whitespace data
    for (i in 1:NUM_COLUMNS) {
        col <- splitColumns[[i]]
        whiteSpaces <- col == ""
        splitColumns[[i]] <- splitColumns[[i]][!whiteSpaces]
    }
    
    
    # create "County" column to track changes in county on this page, delete new county headings from col1
    findCountyChanges <- findCounty(splitColumns[[1]])
    countyColumn <- rep(origCounty, length(splitColumns[[1]])) # origCounty assumed for all by default
    newCountyIndexes <- findCountyChanges[[1]]
    if (newCountyIndexes[1] != -1) { 
        newCountyNames <- findCountyChanges[[2]]
        for (i in 1:length(newCountyIndexes)) {
            if (i + 1 <= length(newCountyIndexes)) {
                for (j in newCountyIndexes[i]:newCountyIndexes[i + 1]) {
                    countyColumn[j] = newCountyNames[i]
                }
            } else {
                for (j in newCountyIndexes[i]:length(countyColumn)) {
                    countyColumn[j] = newCountyNames[i]
                }
            }
        }
        deleteRows <- c(rep(TRUE, length(splitColumns[[1]])))
        for (i in newCountyIndexes) {
            deleteRows[i] <- FALSE
            deleteRows[i + 1] <- FALSE
            deleteRows[i - 1] <- FALSE
        }
        splitColumns[[1]] <- splitColumns[[1]][deleteRows]
        countyColumn <- countyColumn[deleteRows]
    }
    # question: when to add county column?
    # list.prepend(splitColumns, countyColumn) # add county column
    # note: whatever happens to the first column (things added / deleted) must happen to countyColumn
    
    
    # determine whether or not there is a county change on this page
    # meaning data at the top belong to one county, and data at the bottom belong to another
    indexOfTables <- findCountyTotals(splitColumns[[1]])
    if (indexOfTables[1] != -1) {   # if there is a county change, delete county table from all rows
        for (j in 1:NUM_COLUMNS) {
            deleteRows <- c(rep(TRUE, length(splitColumns[[j]])))
            for (i in indexOfTables) {
                deleteRows[i] <- FALSE
                deleteRows[i + 1] <- FALSE
                deleteRows[i + 2] <- FALSE
                deleteRows[i + 3] <- FALSE
                deleteRows[i + 4] <- FALSE
                if (j == 1 | j == 2 | j == 4 | j == 6) {
                    deleteRows[i + 5] <- FALSE
                }
            }
            splitColumns[[j]] <- splitColumns[[j]][deleteRows]
            if (j == 1) {
                countyColumn <- countyColumn[deleteRows]
            }
        }
    } 
    
   
    
    # all column data vectors need to have same length before putting in dataframe
    greatestLength <- 0
    for (i in 1:NUM_COLUMNS) {        # identify the column with the greatest length
        len <- length(splitColumns[[i]])
        if (len > greatestLength) {
            greatestLength <- len
        }
    }
    stringData <- data.frame(Temp = 1:greatestLength)  # dataframe shell
    for (i in 1:NUM_COLUMNS) {
        col = splitColumns[[i]]
        if (length(col) < greatestLength) {
            col <- c(col, rep('#', greatestLength - length(col))) # add '#' characters to fill in any short columns
            if (i == 1) {
                countyColumn <- c(countyColumn, rep('#', greatestLength - length(countyColumn)))
            }
        }
        stringData[paste0("Col", i)] <- col
    }
    stringData <- stringData[-1]

    
    # clean data - remove all periods/commas, replace all lone 'o' or 'O' with '0'
    stringData <- cleanData(stringData, FIRST_NUMERIC_COL, LAST_NUMERIC_COL)
    

    # separating bank and branch names into different columns - deletion part deactivated
    stringData <- separateBanksAndBranches(stringData, origBank)
    
    # add county column
    stringData <- cbind(countyColumn, stringData)
    
    # add and filly state column
    stringData <- cbind(data.frame("State" = c(rep(state, length(stringData$Bank)))), stringData)

    colnames(stringData) <- c("State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
    
    return(stringData)
    
}

#print("------------------ Cleaned Data --------------------")
#print(stringData)

#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)
library(stringr)

########################################################################################
#  Given a list of character vectors containing the data in each column, processes
#  and cleans the data, and returns a dataframe that can be appended to the final
#  note: can now assume that splitColumns will contain data for only one county
#        and any new county will begin at the top
# Parameters:
#   splitColumns - list of character vectors. each vector contains the entries of a column
#   origCounty - string, county of data at top of page
#   state - string, state of data at top of page
#   origBank - string, bank of data at top of page
# Returns:
#   a dataframe that can be directly appended to the final dataframe of extracted data
########################################################################################

source("code/Helper/Helper.R")

NUM_COLUMNS <- 6
FIRST_NUMERIC_COL <- 4
LAST_NUMERIC_COL <- 6

dataProcessing <- function(splitColumns, origCounty, state, origBank) {
    
    # delete unneccessary whitespace data
    for (i in 1:NUM_COLUMNS) {
        col <- splitColumns[[i]]
        whiteSpaces <- col == ""
        splitColumns[[i]] <- splitColumns[[i]][!whiteSpaces]
    }
    
    # 2 cases
    #   1) data in splitColumns all belongs to origCounty
    #   2) data in splitColumns belongs to a new county that will be at the top of the data
    
    # check if this data belongs to origCounty or a new county!
    county <- origCounty
    columnOne <- splitColumns[[1]]
    indexParen1 <- grep("(", columnOne, fixed = TRUE)
    indexParen2 <- grep(")", columnOne, fixed = TRUE)
    indexOfCounty <- intersect(indexParen1, indexParen2)
    if (length(indexOfCounty) != 0) {
        indexOfCounty <- indexOfCounty[1]
        theoreticallyNewCountyLine <- strsplit(columnOne[indexOfCounty], " ")[[1]]
        indexOfParenString <- grep("(", theoreticallyNewCountyLine, fixed = TRUE)
        parenString <- theoreticallyNewCountyLine[indexOfParenString] # edge case: tesseract reads County (003 )
        if (grep(")", theoreticallyNewCountyLine, fixed = TRUE) == indexOfParenString + 1){
            parenString <- paste0(parenString, theoreticallyNewCountyLine[indexOfParenString + 1])
        }
        if (str_length(parenString) == 5) {
            print(paste0("length should be 5, deleting: ", parenString))
            tokens <- indexOfParenString:length(theoreticallyNewCountyLine)
            county <- paste(theoreticallyNewCountyLine[-tokens], collapse = " ") # possibly need to join for multi-word counties?
            deleteRows <- c(rep(TRUE, length(columnOne)))
            deleteRows[indexOfCounty] <- FALSE
            deleteRows[indexOfCounty + 1] <- FALSE # indexOfCounty can be a vector, so this deletes all!
            deleteRows[indexOfCounty - 1] <- FALSE
            splitColumns[[1]] <- columnOne[deleteRows]
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
        }
        stringData[paste0("Col", i)] <- col
    }
    stringData <- stringData[-1]

    
    # clean data - remove all periods/commas, replace all lone 'o' or 'O' with '0'
    stringData <- cleanData(stringData, FIRST_NUMERIC_COL, LAST_NUMERIC_COL)
    

    # separating bank and branch names into different columns - deletion part deactivated
    stringData <- separateBanksAndBranches(stringData, origBank)
    
    # add and fill county column
    stringData <- cbind(data.frame("County" = rep(county, nrow(stringData))), stringData)
    
    # add and fill state column
    stringData <- cbind(data.frame("State" = rep(state, nrow(stringData))), stringData)

    colnames(stringData) <- c("State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
    
    return(stringData)
    
}

#print("------------------ Cleaned Data --------------------")
#print(stringData)

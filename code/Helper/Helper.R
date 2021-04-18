########################################################################################
#  Helper functions for dataProcessing
########################################################################################

# Given the first column as a character vector, searches for the start of a new county
# Returns a list containing:
#   index of county change in column (or -1 if no new county)
#   string containing the name of the new county
findCounty <- function(columnOne) {
    county <- ""
    index <- -1
    indexOfCounty <- grep("(", columnOne, fixed = TRUE)
    if (length(indexOfCounty) != 0) {
        county <- strsplit(columnOne[indexOfCounty], " ")[[1]][1]
        index <- indexOfCounty
    }
    r <- list(index, county)
    return(r)
}


# Given the first column as a character vector, searches for the totals table indicating the end of a county
# Returns index of first row in the table (or -1 if no county change)
findCountyTotals <- function(columnOne) {
    index <- -1
    indexOfTotals <- grep("COUNTY TOTALS", columnOne, fixed = TRUE)
    if (length(indexOfTotals) != 0) {
        index <- indexOfTotals
    }
    return(index)
}


# Cleans the numeric columns of dataframe of extracted text data and returns it
# Specifically, it deletes periods and commas and changes all lone 'o' and 'O's to '0's
cleanData <- function(dirtyDF, firstNumericCol, lastNumericCol) {
    for (i in 1:ncol(dirtyDF)) {
        dirtyDF[i] <- str_replace_all(dirtyDF[[i]], '[.]', ',')
        dirtyDF[i] <- str_replace_all(dirtyDF[[i]], '[,]', '')
        if (i >= firstNumericCol & i <= lastNumericCol) {
            dirtyDF[i] <- str_replace_all(dirtyDF[[i]], '[o]', '0')
            dirtyDF[i] <- str_replace_all(dirtyDF[[i]], '[O]', '0')
        }
    }
    return(dirtyDF)
}




# Given the dataframe of text data, separates banks and branch names into separate columns
# Bank name column appended to the beginning of the dataframe
separateBanksAndBranches <- function(stringData, origBank) {
    stringData <- cbind("Bank" = c(rep(NA, length(stringData$Col1))), stringData)
    curIndex <- 1
    for (i in stringData$Col1) {
        if (grepl("BANK", i) == TRUE | (grepl("MAIN OFFICE", i) == FALSE & grepl("BRANCH",i) == FALSE & grepl("FACILITY", i) == FALSE)) {
            stringData$Bank[curIndex] <- i    # copies bank name into new column
        }
        curIndex <- curIndex + 1
    }
    #assign the value of the bank name to each branch
    curIndex <- 1
    curBank <- origBank
    for (i in stringData$Bank) {
        if (is.na(i) == FALSE) {
            curBank <- i
        } else {
            stringData$Bank[curIndex] <- curBank
        }
        curIndex <- curIndex + 1
    }
    
    #remove bank names and only keep branch names in original first column
    deleteRows <- c(rep(TRUE, length(stringData$Bank)))
    count <- 1
    for (i in 1:length(stringData$Bank)) {
        if (grepl("MAIN OFFICE", stringData$Col1[i]) == FALSE & grepl("BRANCH", stringData$Col1[i]) == FALSE) {
            deleteRows[i] <- FALSE
        }
    }
    stringData <- stringData[deleteRows,]
    colnames(stringData)[2] <- "Branch"
    return(stringData)
}





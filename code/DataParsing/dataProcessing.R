#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Script to brainstorm/test procedure for parsing ocr text and putting data in a dataframe
#  Imports testPDF2, crops into columns, reads columns with OCR, and parses data
#  testPDF2 contains a county break and summary stats in the middle of a page
########################################################################################
# 

#source("code/ColumnCropping/cropColumns.R")
#columns <- cropColumns("1993B1_1.pdf"

#Only applies on first column


dataProcessing <- function(splitColumns, origCounty, state) {
    county <- origCounty
    
    # delete unneccessary whitespace data
    for (i in 1:6) {
        col <- splitColumns[[i]]
        for (j in 1:length(col)) {
            whiteSpaces <- col == ""
        }
        splitColumns[[i]] <- splitColumns[[i]][!whiteSpaces]
    }

    findCounty <- findCounty(splitColumns[[1]])
    
    if (findCounty[[1]] != -1) {
        deleteRows <- c(rep(TRUE, length(splitColumns[[1]])))
        deleteRows[findCounty[[1]]] <- FALSE
        deleteRows[findCounty[[1]] + 1] <- FALSE
        deleteRows[findCounty[[1]] - 1] <- FALSE
        splitColumns[[1]] <- splitColumns[[1]][deleteRows]
        county <- findCounty[[2]]
    }
    
    
    # identify and extract county totals table
    # different numbers of rows will need to be deleted from different columns!
    
    indexOfTable <- findTotals(splitColumns[[1]])
    
    if (indexOfTable != -1) {
        for (j in 1:6) {
            deleteRows <- c(rep(TRUE, length(splitColumns[[j]])))
            deleteRows[indexOfTable] <- FALSE
            deleteRows[indexOfTable + 1] <- FALSE
            deleteRows[indexOfTable + 2] <- FALSE
            deleteRows[indexOfTable + 3] <- FALSE
            deleteRows[indexOfTable + 4] <- FALSE
            if (j == 1 | j == 4) {
                deleteRows[indexOfTable + 5] <- FALSE
            }
            print(deleteRows)
            splitColumns[[j]] <- splitColumns[[j]][deleteRows]
        }
    }
    
    
    
    # all column data vectors need to have same length before putting in dataframe
    greatestLength <- 0
    greatestLengthOrig <- 0
    for (i in 1:length(col_widths)) {        # identify the column with the greatest length
        len <- length(splitColumns[[i]])
        # print(paste0(i, ": ", len))
        if (len > greatestLength) {
            greatestLength <- len
        }
    }
    stringData <- data.frame(Temp = 1:greatestLength)
    #stringDataOrig <- data.frame(Temp = 1:greatestLengthOrig)
    for (i in 1:length(col_widths)) {
        col = splitColumns[[i]]
        #colOrig = splitColumnsOrig[[i]] 
        if (length(col) < greatestLength) {
            col <- c(col, rep('#', greatestLength - length(col))) # add '#' characters to fill in
        }
        # if (length(colOrig) < greatestLengthOrig) {
        #     colOrig <- c(colOrig, rep('#', greatestLengthOrig - length(colOrig))) # add '#' characters to fill in
        # }
        stringData[paste0("Col", i)] <- col
        #stringDataOrig[paste0("Col", i)] <- colOrig
    }
    #stringData <- stringData[-1]
    #stringDataOrig <- stringDataOrig[-1]
    
    
    
    ##TODO: Make County and State Columns
    return(stringData)
    
}
#print("------------------ Original Data --------------------")
#print(stringDataOrig)
#print("------------------ Cleaned Data --------------------")
#print(stringData)

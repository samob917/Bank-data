#install.packages("tesseract")
#install.packages("magick")
#install.packages("pdftools")
#install.packages("stringr")
library(pdftools)
library(magick)
library(tesseract)
library(stringr)

########################################################################################
#  Main driver - extracts data from a multi-page PDF into a dataframe
#  main function takes 3 arguments: path to PDF file, the page number of the first
#  page in the file, and the name of the county of the first page in the file
#
#  IMPORTANT: ensure that working directory is Bank-data
#
#  TO RUN:
#  source("code/main.R")
#  main("test/1993B1_1-7.pdf", 1, "Fairfield")
#
########################################################################################

# format of final dataframe:  
# STATE | COUNTY | BANK | BRANCH | CITY | ZIP | IPC DEPOSITS | ALL OTHER DEPOSITS | TOTAL DEPOSITS

source("code/ColumnCropping/cropColumns.R")
source("code/DataParsing/dataProcessing.R")


main <- function(pdfName, firstPage, firstCounty) {
    image <- image_read_pdf(pdfName, density = 600)
    pageNum <- firstPage
    state <- ""
    county <- firstCounty
    bank <- ""
    finalDF <- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(finalDF) <- c("State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
    
    # will iterate through each page, create a clean dataframe for it, and append it to final
    for (page in 1:length(image)) {
        print(paste0("Processing page ", pageNum, " ------------------------------------------"))
        # note that findCrop will always give you data that is a cont. of current county, or includes new county
        cropColumns <- cropColumns(image[page], pageNum)
        state <- cropColumns[[1]]
        for (i in 2:length(cropColumns)) { # for each vector of extracted text (corresponding to a horizontal cropped section)
            columns <- cropColumns[[i]]
            splitColumns <- strsplit(columns, '\n')         # list of character vectors containing the entries of each column
            toAppend <- dataProcessing(splitColumns, county, state, bank)
            lastIndex <- nrow(toAppend)
            county <- toAppend$County[lastIndex] # reset the county and bank info for next img/page
            bank <- toAppend$Bank[lastIndex]
            # check for data loss, stop if found
            for (col in 1:ncol(toAppend)) {
                lastVal <- toAppend[lastIndex, col]
                if (!is.na(lastVal) & lastVal == "#") {
                    finalDF <- rbind(finalDF, toAppend)
                    fileName <- tail(strsplit(pdfName, '/')[[1]], n = 1)
                    write.csv(finalDF, paste0(fileName, "data.csv"), row.names = FALSE)
                    stop(paste0("probable data loss at page ", pageNum))
                }
            }
            finalDF <- rbind(finalDF, toAppend)
        }

        pageNum <- pageNum + 1
    }
    
    # do the string columns need to be turned into numbers before we write to the csv, or does it not matter?
    fileName <- tail(strsplit(pdfName, '/')[[1]], n = 1)
    write.csv(finalDF, paste0(fileName, "data.csv"), row.names = FALSE)
}


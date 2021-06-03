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
#  Note: on data loss or other error, program saves extracted data in a csv and
#        begins a new csv. Always need to check the end of prev. csv (where the error happened)
#        and beginning of new csv (where prev. state, county, and bank data may be incorrect)
#        there may also be data from prev. page on new csv!
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

# add a pageNum column to every table! for tracking purposes!

main <- function(pdfName, firstPage, firstCounty) {
    # read in image, set initial variables, create dataframe
    image <- image_read_pdf(pdfName, density = 600)
    pageNum <- firstPage
    state <- ""
    county <- firstCounty
    bank <- ""
    finalDF <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(finalDF) <- c("PageNum", "State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
    
    # create folder to store CSVs
    if (dir.exists("output")){
        unlink("output", recursive = TRUE)
    } 
    dir.create("output")
    
    # will iterate through each page, create a clean dataframe for it, and append it to final
    for (page in 1:length(image)) {
        print(paste0("Processing page ", pageNum, " ------------------------------------------"))
        # note that findCrop will always give you data that is a cont. of current county, or includes new county
        cropColumns <- cropColumns(image[page], pageNum)
        if (is.null(cropColumns[[1]]) & is.null(cropColumns[[2]])) { # indicates page cropping failed
            print(paste0("<<< failed to crop page ", pageNum, ", abandoning page and beginning new csv >>>"))
            write.csv(finalDF, paste0("output/pg", firstPage, "-", pageNum - 1, ".csv"), row.names = FALSE)
            firstPage <- pageNum + 1
            finalDF <- data.frame(matrix(ncol = 10, nrow = 0))
            colnames(finalDF) <- c("PageNum", "State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
            pageNum <- pageNum + 1
            next
        }
        state <- cropColumns[[1]]
        for (i in 2:length(cropColumns)) { # for each vector of extracted text (corresponding to a horizontal cropped section)
            columns <- cropColumns[[i]]
            splitColumns <- strsplit(columns, '\n')         # list of character vectors containing the entries of each column
            toAppend <- dataProcessing(splitColumns, county, state, bank)
            toAppend <- cbind(data.frame("PageNum" = rep(pageNum, nrow(toAppend))), toAppend)
            lastIndex <- nrow(toAppend)
            county <- toAppend$County[lastIndex] # reset the county and bank info for next img/page
            bank <- toAppend$Bank[lastIndex]
            finalDF <- rbind(finalDF, toAppend) # append new data to finalDF
            # check for data loss, stop if found
            for (col in 1:ncol(toAppend)) {
                lastVal <- toAppend[lastIndex, col]
                if (!is.na(lastVal) & lastVal == "#") {
                    write.csv(finalDF, paste0("output/pg", firstPage, "-", pageNum, ".csv"), row.names = FALSE)
                    #stop(paste0("probable data loss at page ", pageNum))
                    print(paste0("<<< probable data loss at page ", pageNum, ", beginning new csv >>>"))
                    firstPage <- pageNum + 1
                    finalDF <- data.frame(matrix(ncol = 10, nrow = 0))
                    colnames(finalDF) <- c("PageNum", "State", "County", "Bank", "Branch", "City", "ZIP", "IPC Deposits", "All Other Deposits", "Total Deposits")
                    break
                }
            }
        }

        pageNum <- pageNum + 1
    }
    
    #fileName <- tail(strsplit(pdfName, '/')[[1]], n = 1)
    #write.csv(finalDF, paste0(fileName, "data.csv"), row.names = FALSE)
    if (firstPage < pageNum) {
        write.csv(finalDF, paste0("output/pg", firstPage, "-", pageNum - 1, ".csv"), row.names = FALSE)
    }
}


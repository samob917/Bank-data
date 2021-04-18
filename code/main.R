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
#  IMPORTANT: ensure that working directory is Bank-data
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
        cropColumns <- cropColumns(image[page], pageNum)
        state <- cropColumns[[2]]
        columns <- cropColumns[[1]]
        
        splitColumns <- strsplit(columns, '\n')         # list of character vectors containing the entries of each column
        toAppend <- dataProcessing(splitColumns, county, state, bank)
        county <- toAppend$County[length(toAppend$County) - 1]
        bank <- toAppend$Bank[length(toAppend$Bank) - 1]
        finalDF <- rbind(finalDF, toAppend)
        pageNum <- pageNum + 1
    }
    
    # do the string columns need to be turned into numbers before we write to the csv, or does it not matter?
    
    write.csv(finalDF, "bankData.csv", row.names = FALSE)
}


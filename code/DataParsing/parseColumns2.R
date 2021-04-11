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
# source("ColumnCropping/cropPage.R")
# 
# pageNum <- 12
# 
# # stores percentage that each column should take up
# col_pct <- c(0.36530, 0.15720, 0.09054, 0.12356, 0.09906, 0.04261)
# 
# columns <- vector("character", 6)                 # will hold OCR-extracted text for each column
# 
# if (!file.exists("DataParsing/testPDF2cols")) { # create a folder to hold crops for this page
#     dir.create("DataParsing/testPDF2cols")
# }
# 
# img <- image_read_pdf("DataParsing/testPDF2.pdf", density = 600)
# cropPageReturns <- cropPage(img, pageNum)
# cropped <- cropPageReturns[[1]]
# image_write(cropped, path = paste0("DataParsing/testPDF2cols/cropped.pdf"), format = "pdf") # saved cropped page to folder
# width <- as.numeric(image_info(cropped)[2]) # reset width and height for cropped page
# height <- as.numeric(image_info(cropped)[3])
# col_widths <- c(0, 0, 0, 0, 0, 0) # numeric vector to store pixel widths for each column
# for (j in 1:length(col_pct)) {
#     col_widths[j] <- col_pct[j] * width
#     # print(col_widths)
#     if (j == length(col_pct)) {
#         geo <- paste0(width - sum(col_widths[1:j-1]) - 100, 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
#     } else {
#         geo <- paste0(col_widths[j], 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
#     }
#     column <- image_crop(cropped, geo)
#     columns[j] <- ocr(column, engine = tesseract("eng"))
#     image_write(column, path = paste0("DataParsing/testPDF2cols/col", j, ".pdf"), format = "pdf")
# }

source("code/ColumnCropping/cropColumns.R")
columns <- cropColumns("1993B1_1.pdf")

splitColumns <- strsplit(columns, '\n')         # list of vectors containing split column data
splitColumnsOrig <- splitColumns                # will keep track of how data originally looked

# extract and identify county, delete those rows from col1 - assumes there is a county header
indexOfParen <- grep("(", splitColumns[[1]], fixed = TRUE)
#print(splitColumns[[1]])
#print(indexOfParen)
county <- strsplit(splitColumns[[1]][indexOfParen], " ")[[1]][1]
print(county)                 # put this in appropriate place in final dataframe!!
deleteRows <- c(rep(TRUE, length(splitColumns[[1]])))
deleteRows[indexOfParen] <- FALSE
deleteRows[indexOfParen + 1] <- FALSE
deleteRows[indexOfParen - 1] <- FALSE
splitColumns[[1]] <- splitColumns[[1]][deleteRows]

# delete unneccessary whitespace data
for (i in 1:length(col_widths)) {
    col <- splitColumns[[i]]
    for (j in 1:length(col)) {
        whiteSpaces <- col == ""
    }
    splitColumns[[i]] <- splitColumns[[i]][!whiteSpaces]
}

# identify and extract county totals table
# different numbers of rows will need to be deleted from different columns!
indexOfTable <- grep("COUNTY TOTALS", splitColumns[[1]], fixed = TRUE)
print(indexOfTable)
for (j in 1:length(col_pct)) {
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

# all column data vectors need to have same length before putting in dataframe
greatestLength <- 0
greatestLengthOrig <- 0
for (i in 1:length(col_widths)) {        # identify the column with the greatest length
    len <- length(splitColumns[[i]])
    # print(paste0(i, ": ", len))
    if (len > greatestLength) {
        greatestLength <- len
    }
    len <- length(splitColumnsOrig[[i]])
    # print(paste0(i, ": ", len))
    if (len > greatestLengthOrig) {
        greatestLengthOrig <- len
    }
}
stringData <- data.frame(Temp = 1:greatestLength)
stringDataOrig <- data.frame(Temp = 1:greatestLengthOrig)
for (i in 1:length(col_widths)) {
    col = splitColumns[[i]]
    colOrig = splitColumnsOrig[[i]] 
    if (length(col) < greatestLength) {
        col <- c(col, rep('#', greatestLength - length(col))) # add '#' characters to fill in
    }
    if (length(colOrig) < greatestLengthOrig) {
        colOrig <- c(colOrig, rep('#', greatestLengthOrig - length(colOrig))) # add '#' characters to fill in
    }
    stringData[paste0("Col", i)] <- col
    stringDataOrig[paste0("Col", i)] <- colOrig
}
stringData <- stringData[-1]
stringDataOrig <- stringDataOrig[-1]

print("------------------ Original Data --------------------")
print(stringDataOrig)
print("------------------ Cleaned Data --------------------")
print(stringData)

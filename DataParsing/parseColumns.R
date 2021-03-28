#install.packages("pdftools")
#install.packages("magick")
#install.packages("tesseract")
library(pdftools)
library(magick)
library(tesseract)

########################################################################################
#  Script to brainstorm/test procedure for parsing ocr text and putting data in a dataframe
#  Imports testPDF, crops into columns, reads columns with OCR, and parses data
#    
########################################################################################

source("ColumnCropping/cropPage.R")

pageNum <- 1

# stores percentage that each column should take up
col_pct <- c(0.36643, 0.15765, 0.09054, 0.12356, 0.09906, 0.04261)

columns <- vector("character", 6)                 # will hold OCR-extracted text for each column

if (!file.exists("DataParsing/testPDFcols")) { # create a folder to hold crops for this page
    dir.create("DataParsing/testPDFcols")
}

img <- image_read_pdf("DataParsing/testPDF.pdf", density = 600)
cropPageReturns <- cropPage(img, pageNum)
cropped <- cropPageReturns[[1]]
image_write(cropped, path = paste0("DataParsing/testPDFcols/cropped.pdf"), format = "pdf") # saved cropped page to folder
width <- as.numeric(image_info(cropped)[2]) # reset width and height for cropped page
height <- as.numeric(image_info(cropped)[3])
col_widths <- c(0, 0, 0, 0, 0, 0) # numeric vector to store pixel widths for each column
for (j in 1:length(col_pct)) {
    col_widths[j] <- col_pct[j] * width
    # print(col_widths)
    if (j == length(col_pct)) {
        geo <- paste0(width - sum(col_widths[1:j-1]) - 100, 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
    } else {
        geo <- paste0(col_widths[j], 'x', height, '+', sum(col_widths[1:j]) - col_widths[j], '+', 0)
    }
    column <- image_crop(cropped, geo)
    columns[j] <- ocr(column, engine = tesseract("eng"))
    image_write(column, path = paste0("DataParsing/testPDFcols/col", j, ".pdf"), format = "pdf")
}



splitColumns <- strsplit(columns, '\n')         # list of vectors containing split column data
splitColumnsOrig <- splitColumns                # will keep track of how data originally looked

# extract and identify county, delete those rows from col1
indexOfParen <- grep("(", splitColumns[[1]], fixed = TRUE)
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



"
create a dataframe to store data
Case 1: regular data, no county breaks
for each line:
    convert data in each column into appropriate type, store in dataframe
    move to next line in all columns

Case 2: county change and summary statistics

use regex to search for:  (xxx)
if found:
    find the next set of parenthesis (to show where this county data ends)
    delete row above and below parenthesis
    pull county name from line with parenthesis in it, fill in dataframe

final dataframe: year  county  bank name  branch name   zipcode   IPC Deposits   All other deposits   Total deposits

TO DO

1) Data cleaning - write a new script that takes in a dataframe and cleans it (Sam)
    - Iterate through last 3 columns, identify periods and change them to commas
    - Change o's to 0s
    - Possibly flagging unrecognized characters?

2) Create final dataframe, populate with year, county, etc

3) Write code to differentiate banks and branches (can check zip code column for letters or numbers)  (Roma)
   Once you've identified a row containing bank info, put it in separate dataframe
   
4) Create new R script to process page that has a county break (Casey)



"


